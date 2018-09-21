{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Server (server) where

import Common

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Network.Socket
import Network.Socket.ByteString as ByteStringSocket
import Network.HostName as NH

import System.Clock as SC
import Data.Time.Clock

type HandlerFunc = SockAddr -> B.ByteString -> String -> IO ()

server :: String -> IO ()
server udpPort = serveLog udpPort plainHandler

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
                 (msg, addr) <- ByteStringSocket.recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg hostname
                 -- And process more messages
                 procMessages sock hostname

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler _ msg hostname =
    do
#ifdef mingw32_HOST_OS
      monotonicTimeCounter <- getTime Monotonic -- FIXME: Change to MonotonicRaw on Linux
#else
      monotonicTimeCounter <- getTime MonotonicRaw -- FIXME: Change to MonotonicRaw on Linux
#endif
      currentTime <- getCurrentTime
      putStrLn $ (show currentTime) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter) ++ "," ++ (C.unpack msg)
