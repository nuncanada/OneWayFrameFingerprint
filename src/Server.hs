{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Server (main) where

import Shared

import Data.List
import Data.Map as DM
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C

import Control.Applicative

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

--import Network.DNS.Lookup
--import Network.DNS
--import qualified Network.Info as NI

--import System.Environment

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

maxQueueSize :: Int
maxQueueSize = 5


data ServerSocket =
    ServerHandle {slSocket :: Socket,
                  slAddress :: SockAddr} deriving Show

data ServerHandle =
    SyslogHandle {slHandle :: Handle,
                  slProgram :: String} deriving Show

type HandlerFunc = B.ByteString -> SockAddr -> B.ByteString -> IPv4 -> IO ()

newtype ServerPort = String

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Syntax: OneWayFrameFingerprintServer serverPort"
         exitFailure

  serverPort <- (read $ head args :: ServerPort)
  hostname <- NH.getHostName
  serverList <- servers
  let logger = timeRotate $ hostname <> ".log"
  timeCache <- newTimeCache timeFormat
  (logger', cleanup) <- newTimedFastLogger timeCache logger
  let logFunction = logLogger logger'
  putStrLn ("hostname: " ++ (show hostname))
  putStrLn "Server list:"
  putStrLn (show serverList)
  server serverPort hostAddress logFunction

server :: ServerPort -> IPv4 -> (LogStr -> IO ()) -> IO ()
server port localAddress log = serveLog port localAddress (plainHandler log)

serveLog :: ServerPort
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
plainHandler logger "PING" address msg localAddress = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      let responseMessage = "PONG " ++ (show currentTime) ++ "," ++ (show localAddress) ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg)
      sendMessage responseMessage address handles
      logger responseMessage
      return ()
plainHandler log "PONG" _ msg localAddress = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      log $ toLogStr ("RECV " ++ (show currentTime) ++ "," ++ (show localAddress) ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg))
      return ()
plainHandler log command address msg localAddress = do
      log $ toLogStr ("Comando desconhecido: " ++ (B.unpack command) ++ " - msg: " ++ (B.unpack msg) ++ " - localAddress: " ++ (show localAddress) ++ " - address: " ++ (show address))
      return ()


{-

library
  default-language:    Haskell2010
  hs-source-dirs:      src/
  build-depends:       base >=4.9 && <4.12,
                       network >= 2.6,
                       network-info >= 0.2,
                       hostname >= 1.0,
                       bytestring >= 0.10,
                       clock >= 0.7,
                       formatting >= 6.3,
                       time >= 1.8,
                       dns >= 3.0.4,
                       iproute >= 1.7.6,
                       network-info >= 0.2,
                       containers >= 0.5.7,
                       fast-logger >= 2.4.12,
                       process >= 1.6.5,
                       cassava >= 0.5.0,
                       vector >= 0.12
}
