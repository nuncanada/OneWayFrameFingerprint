{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List
import Data.List.Split
import Data.Map as DM
import qualified Data.ByteString.Char8 as B

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

--import Network.DNS.Lookup
--import Network.DNS
--import qualified Network.Info as NI

--import System.Environment
import System.Clock as SC
import Data.Time.Clock
import Data.IP

import Control.Concurrent

import Text.Printf
import Debug.Trace

import System.Log.FastLogger
import System.Process

import Data.Monoid
import Data.Function
import Control.Monad

import System.Environment

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

timeFormat :: TimeFormat
timeFormat = "%F"

logLogger :: TimedFastLogger -> LogStr -> IO ()
logLogger logger msg = logger (\_ -> msg <> "\n")

timeRotate :: FilePath -> LogType
timeRotate fname = LogFileTimedRotate
               (TimedFileLogSpec fname timeFormat sametime compressFile)
               defaultBufSize
   where
       sametime = (==) --`on` B.takeWhile (/='T')) x)
       compressFile fp = void . forkIO $
           callProcess "gzip" [ fp ]


--cloudServers :: Map String IPv4
--cloudServers = fromList([("TRABALHO-FLAVIO",   (read "127.0.0.1" :: IPv4))])

{-
cloudServers :: Map String IPv4
cloudServers = fromList([
      ("ashburn-a",    (read "10.150.0.2" :: IPv4)),
      ("losangeles-a", (read "10.168.0.2" :: IPv4)),
      ("quebec-a",     (read "10.162.0.2" :: IPv4)),
      ("saopaulo-a",   (read "10.158.0.2" :: IPv4)),
      ("thedalles-a",  (read "10.138.0.2" :: IPv4))
  ])
-}

serverVectorToMap :: V.Vector (String, String, Int) -> Map String (IPv4, Int)
serverVectorToMap = V.foldr (
    \(nome, ipString, porta) m -> DM.insert nome ((read ipString)::IPv4, porta) m
  ) empty

{-
readServerFile :: [String] -> Map String (IPv4, Int)
readServerFile = Data.List.foldr (\s m ->
    let
      (nome:ip:porta:_) = (splitWhen (==',') s)
      n = read nome :: String
      i = read ip :: IPv4
      p = read porta :: Int
    in DM.insert n (i,p) m) empty
-}

data ServerHandle =
    ServerHandle {slSocket :: Socket,
                  slAddress :: SockAddr} deriving Show

type HandlerFunc = B.ByteString -> SockAddr -> B.ByteString -> String -> IO ()

main :: IO ()
main = do
    args <- getArgs;
    progName <- getProgName;
    hostname <- NH.getHostName;
    putStrLn $ "args.length: " ++ (show (length args));
    print args
    if ((length args) /= 4) then do {
        putStrLn $ "usage: " ++ progName ++ " <port> <timeBetweenPings> <logDirectory> <serverCsvFile>";
    } else do
        putStrLn ("hostname: " ++ (show hostname));
        let port = read (args !! 0) :: PortNumber;
        let timeBetweenPings = read (args !! 1) :: Float;
        let logDirectory = args !! 2;
        let serverCsvFile = args !! 3;
        let logger = timeRotate $ logDirectory ++ "/" ++ hostname ++ ".log";
        let waitBetweenPings = round $ timeBetweenPings * 1000 * 1000;

        csvData <- BL.readFile serverCsvFile
        case decode NoHeader csvData of
            Left err -> putStrLn err
            Right v -> do
              let cloudServers = serverVectorToMap v;
              if not (DM.member hostname cloudServers) then do
                putStrLn $ "hostname not found in " ++ serverCsvFile;
              else do
                  let hostAddress = cloudServers ! hostname;
                  timeCache <- newTimeCache timeFormat;
                  (logger', cleanup) <- newTimedFastLogger timeCache logger;
                  let log = logLogger logger';
                  putStrLn (show hostAddress);
                  _ <- forkIO $ client cloudServers hostname waitBetweenPings;
                  server port hostname log;


getHostAddress :: SockAddr -> HostAddress
getHostAddress (SockAddrInet _ hostAddress) = hostAddress
getHostAddress _ = error "Nao implementado"

showTimeSpec :: TimeSpec -> String
showTimeSpec   (TimeSpec  (toInteger -> s) (toInteger -> n)) = (show s) ++ "."  ++ (printf "%09d" n)

openlog :: (IPv4, Int) -> IO ServerHandle
openlog hostAddress =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just (show $ fst hostAddress)) (Just $ show (snd hostAddress))
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, and server address in a handle
       return $ ServerHandle sock (addrAddress serveraddr)

openSockets :: Map String (IPv4, Int) -> IO [ServerHandle]
openSockets servers =
    mapM (\address -> openlog address) (elems servers)


server :: PortNumber -> HostName -> (LogStr -> IO ()) -> IO ()
server port hostname log = serveLog port hostname (plainHandler log)

serveLog :: PortNumber -> HostName -> HandlerFunc -> IO ()
serveLog port hostname handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just $ show port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bind sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, addr) <- SB.recvFrom sock 1024
                 -- Handle it
                 let command = B.take 4 msg
                 handlerfunc command addr msg hostname
                 -- And process more messages
                 procMessages sock

getOsMonotonicCounter :: IO TimeSpec
getOsMonotonicCounter =
  do
#ifdef mingw32_HOST_OS
    monotonicTimeCounter <- getTime Monotonic
#else
    monotonicTimeCounter <- getTime MonotonicRaw
#endif
    return monotonicTimeCounter

sendMessage' :: ServerHandle -> String -> IO ()
sendMessage' handle response = do {
  sendstr response handle
  ; return ()
}

sendMessage :: String -> SockAddr -> [ServerHandle] -> IO ()
sendMessage response address handles =
    let hostHandle = find (\handle -> (getHostAddress address) == (getHostAddress $ slAddress handle)) handles in
      case hostHandle of
        Nothing -> trace ((show handles) ++ "-" ++ (show address)) $ trace (show hostHandle) $ trace (show (getHostAddress address)) $ error "Unable to find handle"
        Just handle -> (sendMessage' handle response)

plainHandler :: (LogStr -> IO ()) -> HandlerFunc
plainHandler log "PING" _ msg hostname = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      log $ toLogStr ("RECV " ++ (show currentTime) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (B.unpack msg))
      return ()
plainHandler log command address msg hostname = do
      log $ toLogStr ("Comando desconhecido: " ++ (B.unpack command) ++ " - msg: " ++ (B.unpack msg) ++ " - hostname: " ++ hostname ++ " - address: " ++ (show address))
      return ()

client :: Map String (IPv4, Int) -> String -> Int -> IO ()
client cloudServers hostname waitBetweenPings =
  do
    handles <- openSockets cloudServers;
    sendTimeLoop handles 0 hostname waitBetweenPings
--    mapM_ closelog handles

sendTimeLoop :: [ServerHandle] -> Integer -> String -> Int -> IO ()
sendTimeLoop handles index hostname waitBetweenPings = do
  threadDelay waitBetweenPings
  mapM_ (\x -> sendTime x index hostname) handles
  sendTimeLoop handles (index+1) hostname waitBetweenPings

sendmsg :: Show a => a -> String -> IO [Char]
sendmsg index hostname = do
  monotonicTimeCounter <- getOsMonotonicCounter
  currentTime <- getCurrentTime
  return $ "PING " ++ (show currentTime) ++ "," ++ (show index) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter)

sendstr :: String -> ServerHandle -> IO ()
sendstr []   _      = return ()
sendstr omsg handle = do
  sent <- SB.sendTo (slSocket handle) (B.pack omsg) (slAddress handle)
  sendstr (genericDrop sent omsg) handle

sendTime :: ServerHandle -> Integer -> String -> IO ()
sendTime handle index hostname = do
  omsg <- sendmsg index hostname
  sendstr omsg handle

closelog :: ServerHandle -> IO ()
closelog handle = close (slSocket handle)
