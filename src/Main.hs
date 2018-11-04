{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.List
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

import System.Clock as SC
import Data.Time.Clock

import Control.Concurrent

import Text.Printf
import Debug.Trace

cloudServers :: [String]
--cloudServers = ["TRABALHO-FLAVIO"]
cloudServers = ["losangeles", "quebec", "saopaulo", "thedalles", "ashburn"]

udpPort :: String
udpPort = "1905"

data ServerHandle =
    ServerHandle {slHostName :: HostName,
                  slSocket :: Socket,
                  slAddress :: SockAddr} deriving Show

type HandlerFunc = B.ByteString -> SockAddr -> B.ByteString -> String ->  IO ()

showTimeSpec :: TimeSpec -> String
showTimeSpec   (TimeSpec  (toInteger -> s) (toInteger -> n)) = (show s) ++ "."  ++ (printf "%09d" n)

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IO ServerHandle      -- ^ Handle to use for logging
openlog hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, and server address in a handle
       return $ ServerHandle hostname sock (addrAddress serveraddr)

openSockets :: [String] -> String -> IO [ServerHandle]
openSockets servers port =
    mapM (\x -> openlog x port) servers

main :: IO ()
main = do
    handles <- openSockets cloudServers udpPort
    _ <- forkIO $ client handles
    server udpPort handles

server :: String -> [ServerHandle] -> IO ()
server port handles = serveLog port (plainHandler handles)

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       hostname <- NH.getHostName
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bind sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock hostname
    where procMessages sock hostname =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, addr) <- SB.recvFrom sock 1024
                 -- Handle it
                 let command = B.take 4 msg
                 handlerfunc command addr msg hostname
                 -- And process more messages
                 procMessages sock hostname

getOsMonotonicCounter :: IO TimeSpec
getOsMonotonicCounter =
  do
#ifdef mingw32_HOST_OS
    monotonicTimeCounter <- getTime Monotonic
#else
    monotonicTimeCounter <- getTime MonotonicRaw
#endif
    return monotonicTimeCounter

sendMessage' :: Maybe ServerHandle -> String -> IO ()
sendMessage' Nothing       _        = error "Nothing in handle"
sendMessage' (Just handle) response = do {
  sendstr response handle
  ; return ()
}

sendMessage :: String -> HostName -> [ServerHandle] -> IO ()
sendMessage response hostname handles =
    let hostHandle = find (\handle -> hostname == slHostName handle) handles in
      --trace (show handles)
      --trace (show hostHandle)
      --trace (show hostname)
      (sendMessage' hostHandle response)

plainHandler :: [ServerHandle] -> HandlerFunc
plainHandler handles "PING" _ msg hostname = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      let responseMessage = "PONG " ++ (show currentTime) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg)
      sendMessage responseMessage hostname handles
      putStrLn responseMessage
      return ()
plainHandler _ "PONG" _ msg hostname = do
      monotonicTimeCounter <- getOsMonotonicCounter
      currentTime <- getCurrentTime
      putStrLn $ "RECV " ++ (show currentTime) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg)
      return ()
plainHandler _ command _ msg hostname = do
    putStrLn $ "Comando desconhecido: " ++ (B.unpack command) ++ " - msg: " ++ (B.unpack msg) ++ " - hostname: " ++ hostname
    return ()

client :: [ServerHandle] -> IO ()
client handles =
  do
    sendTimeLoop handles 0
    mapM_ closelog handles

sendTimeLoop :: [ServerHandle] -> Integer -> IO ()
sendTimeLoop handles index = do
  threadDelay (100 * 1000)
  mapM_ (\x -> sendTime x index) handles
  sendTimeLoop handles (index+1)

sendmsg :: Show a => a -> IO [Char]
sendmsg index = do
  hostname <- NH.getHostName
  monotonicTimeCounter <- getOsMonotonicCounter
  currentTime <- getCurrentTime
  return $ "PING " ++ (show currentTime) ++ "," ++ (show index) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter)

sendstr :: String -> ServerHandle -> IO ()
sendstr []   _      = return ()
sendstr omsg handle = do
  sent <- SB.sendTo (slSocket handle) (B.pack omsg) (slAddress handle)
  sendstr (genericDrop sent omsg) handle

sendTime :: ServerHandle -> Integer -> IO ()
sendTime handle index = do
  omsg <- sendmsg index
  sendstr omsg handle

closelog :: ServerHandle -> IO ()
closelog handle = close (slSocket handle)
