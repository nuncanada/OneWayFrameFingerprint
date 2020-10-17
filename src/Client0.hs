{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Client (main) where

import Data.List
import Data.Map as DM
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

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

import Data.Either

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


data ServerEntry =
    ServerEntry {
        name :: !String
      , address :: !String
      , port :: !Int
    } deriving (Show)

instance FromNamedRecord ServerEntry where
    parseNamedRecord r = ServerEntry <$> r .: "name" <*> r .: "address" <*> r .: "port"

servers :: IO ([ServerEntry])
servers = do
  csvData <- BL.readFile "servers.txt"
  return (returnServers $ decodeByName csvData)

returnServers :: Either String (Header, V.Vector ServerEntry) -> [ServerEntry]
returnServers (Left _) = []
returnServers (Right (_, v)) = V.toList v

data ServerSocket =
    ServerHandle {slSocket :: Socket,
                  slAddress :: SockAddr} deriving Show

data ServerHandle =
    SyslogHandle {slHandle :: Handle,
                  slProgram :: String} deriving Show

type HandlerFunc = B.ByteString -> SockAddr -> B.ByteString -> IPv4 -> IO ()


main :: IO ()
main = do
  hostname <- NH.getHostName
  putStrLn ("hostname: " ++ (show hostname))
  serverList <- servers
  putStrLn (show serverList)
  let logger = timeRotate $ hostname <> ".log"
  let hostAddress = cloudServers ! hostname
  timeCache <- newTimeCache timeFormat
  (logger', cleanup) <- newTimedFastLogger timeCache logger
  let logFunction = logLogger logger'
  putStrLn ("hostAddress: " ++ show hostAddress)
  handles <- openSockets serverList
  _ <- forkIO $ client handles hostAddress
  server tcpPort handles hostAddress logFunction

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

       setSocketOption sock KeepAlive 1

       h <- socketToHandle sock WriteMode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h (BlockBuffering Nothing)

       -- Save off the socket, and server address in a handle
       return $ ServerHandle h (addrAddress serveraddr) ++ ":" ++

openSockets :: [ServerEntry] -> IO [ServerHandle]
openSockets servers =
    mapM (\server -> openlog (address server) (portserver)) servers

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
       bindSocket sock (addrAddress serveraddr)

       -- Start listening for connection requests.  Maximum queue size
       -- of 5 connection requests waiting to be accepted.
       listen sock maxQueueSize

       -- Create a lock to use for synchronizing access to the handler
       lock <- newMVar ()

       -- Loop forever waiting for connections.  Ctrl-C to abort.
       procRequests lock sock
    where
      procRequests :: MVar () -> Socket -> IO ()
      procRequests lock mastersock =
          do (connsock, clientaddr) <- accept mastersock
             handle lock clientaddr
                "syslogtcpserver.hs: client connnected: " ++ (show clientaddr)
             forkIO $ procMessages lock connsock clientaddr
             procRequests lock mastersock

      -- | Process incoming messages
      procMessages :: MVar () -> Socket -> SockAddr -> IO ()
      procMessages lock connsock clientaddr =
          do connhdl <- socketToHandle connsock ReadMode
             hSetBuffering connhdl LineBuffering
             messages <- hGetContents connhdl
             mapM_ (handle lock clientaddr) (lines messages)
             hClose connhdl
             handle lock clientaddr
                "syslogtcpserver.hs: client disconnected: " ++ (show clientaddr)

      -- Lock the handler before passing data to it.
      handle :: MVar () -> HandlerFunc
      -- This type is the same as
      -- handle :: MVar () -> SockAddr -> String -> IO ()
      handle lock clientaddr msg =
          withMVar lock
             (\a -> handlerfunc clientaddr msg >> return a)

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
