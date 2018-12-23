{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.List
import Data.Map as DM
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C

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

timeFormat :: TimeFormat
timeFormat = "%F"

waitBetweenPings :: Int
waitBetweenPings = 500 * 1000

logLogger :: TimedFastLogger -> LogStr -> IO ()
logLogger logger msg = logger (\_ -> msg <> "\n")

timeRotate :: FilePath -> LogType
timeRotate fname = LogFileTimedRotate
               (TimedFileLogSpec fname timeFormat sametime compressFile)
               defaultBufSize
   where
       sametime = (==) --`on` C.takeWhile (/='T')) x)
       compressFile fp = void . forkIO $
           callProcess "gzip" [ fp ]


cloudServers :: Map String IPv4
cloudServers = fromList([("TRABALHO-FLAVIO",   (read "127.0.0.1" :: IPv4))])

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

udpPort :: String
udpPort = "1905"

data ServerHandle =
    ServerHandle {slSocket :: Socket,
                  slAddress :: SockAddr} deriving Show

type HandlerFunc = B.ByteString -> SockAddr -> B.ByteString -> IPv4 -> IO ()

main :: IO ()
main = do
    hostname <- NH.getHostName
    putStrLn ("hostname: " ++ (show hostname))
    let logger = timeRotate $ "/home/fezsentido/" <> hostname <> ".log"
    let hostAddress = cloudServers ! hostname
    timeCache <- newTimeCache timeFormat
    (logger', cleanup) <- newTimedFastLogger timeCache logger
    let log = logLogger logger'
    putStrLn (show hostAddress)
    handles <- openSockets cloudServers udpPort
    _ <- forkIO $ client handles hostAddress
    server udpPort handles hostAddress log

getHostAddress :: SockAddr -> HostAddress
getHostAddress (SockAddrInet _ hostAddress) = hostAddress
getHostAddress _ = error "Nao implementado"

showTimeSpec :: TimeSpec -> String
showTimeSpec   (TimeSpec  (toInteger -> s) (toInteger -> n)) = (show s) ++ "."  ++ (printf "%09d" n)

openlog :: IPv4                 -- ^ HostAddress
        -> String               -- ^ Port number or name; 514 is default
        -> IO ServerHandle      -- ^ Handle to use for logging
openlog hostAddress port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just (show hostAddress)) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, and server address in a handle
       return $ ServerHandle sock (addrAddress serveraddr)

openSockets :: Map String IPv4 -> String -> IO [ServerHandle]
openSockets servers port =
    mapM (\x -> openlog x port) (elems servers)


server :: String -> [ServerHandle] -> IPv4 -> (LogStr -> IO ()) -> IO ()
server port handles localAddress log = serveLog port localAddress (plainHandler handles log)

serveLog :: String              -- ^ Port number or name; 514 is default
         -> IPv4
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port localAddress handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
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
                 handlerfunc command addr msg localAddress
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

plainHandler :: [ServerHandle] -> (LogStr -> IO ()) -> HandlerFunc
plainHandler handles _ "PING" address msg localAddress = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      let responseMessage = "PONG " ++ (show currentTime) ++ "," ++ (show localAddress) ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg)
      sendMessage responseMessage address handles
      --This was for debugging packet loss. Optmizing for smaller log files now.
      --log responseMessage
      return ()
plainHandler _ log "PONG" _ msg localAddress = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      log $ toLogStr ("RECV " ++ (show currentTime) ++ "," ++ (show localAddress) ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg))
      return ()
plainHandler _ log command address msg localAddress = do
      log $ toLogStr ("Comando desconhecido: " ++ (B.unpack command) ++ " - msg: " ++ (B.unpack msg) ++ " - localAddress: " ++ (show localAddress) ++ " - address: " ++ (show address))
      return ()

client :: [ServerHandle] -> IPv4 -> IO ()
client handles localAddress =
  do
    sendTimeLoop handles 0 localAddress
--    mapM_ closelog handles

sendTimeLoop :: [ServerHandle] -> Integer -> IPv4 -> IO ()
sendTimeLoop handles index localAddress = do
  threadDelay waitBetweenPings
  mapM_ (\x -> sendTime x index localAddress) handles
  sendTimeLoop handles (index+1) localAddress

sendmsg :: Show a => a -> IPv4 -> IO [Char]
sendmsg index localAddress = do
  monotonicTimeCounter <- getOsMonotonicCounter
  currentTime <- getCurrentTime
  return $ "PING " ++ (show currentTime) ++ "," ++ (show index) ++ "," ++ (show localAddress) ++ "," ++ (showTimeSpec monotonicTimeCounter)

sendstr :: String -> ServerHandle -> IO ()
sendstr []   _      = return ()
sendstr omsg handle = do
  sent <- SB.sendTo (slSocket handle) (B.pack omsg) (slAddress handle)
  sendstr (genericDrop sent omsg) handle

sendTime :: ServerHandle -> Integer -> IPv4 -> IO ()
sendTime handle index localAddress = do
  omsg <- sendmsg index localAddress
  sendstr omsg handle

closelog :: ServerHandle -> IO ()
closelog handle = close (slSocket handle)
