{-# LANGUAGE OverloadedStrings #-}

module Server (main) where

import Common

import Data.Bits
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Network.Socket
import Network.Socket.ByteString as ByteStringSocket
import Network.HostName as NH

import Control.Concurrent
import Control.Monad
import System.Exit
import Data.Char (toLower)
import System.IO

import System.Clock as SC

type HandlerFunc = SockAddr -> B.ByteString -> IO ()

main :: IO ()
main = do
    forkIO realMain
    exitOnQ

exitOnQ :: IO ()
exitOnQ = do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (toLower c /= 'q') exitOnQ
    exitSuccess  -- or "exitWith" and some ExitCode value, use hoogle.

realMain :: IO ()
realMain = serveLog "1905" plainHandler

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
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
                 (msg, addr) <- ByteStringSocket.recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler (SockAddrInet port addr) msg = putStrLn $ "From " ++ show addr ++ ": " ++ (C.unpack msg)
plainHandler addr msg =
    do hostname <- NH.getHostName
       currentTime <- getTime Monotonic -- FIXME: Change to MonotonicRaw on Linux
       putStrLn $ hostname ++ "," ++ (showTimeSpec currentTime) ++ "," ++ (C.unpack msg)
