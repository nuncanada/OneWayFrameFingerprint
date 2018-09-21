{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Client (client) where

import Common

import Data.List
import qualified Data.ByteString.Char8 as B

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

import System.Clock as SC
import Data.Time.Clock

import Control.Concurrent

data ServerHandle =
    ServerHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

client :: [String] -> String -> IO ()
client servers udpPort =
  do
    handles <- openSockets servers udpPort
    sendTimeLoop handles 0
    mapM_ closelog handles

sendTimeLoop :: [ServerHandle] -> Integer -> IO ()
sendTimeLoop handles index = do
  threadDelay (100 * 1000)
  mapM_ (\x -> sendTime x index) handles
  sendTimeLoop handles (index+1)

openSockets :: [String] -> String -> IO [ServerHandle]
openSockets servers port =
    mapM (\x -> openlog x port) servers

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
       return $ ServerHandle sock (addrAddress serveraddr)

sendTime :: ServerHandle -> Integer -> IO ()
sendTime syslogh index =
    sendstr' sendmsg
    where sendmsg = do hostname <- NH.getHostName
#ifdef mingw32_HOST_OS
                       monotonicTimeCounter <- getTime Monotonic -- FIXME: Change to MonotonicRaw on Linux
#else
                       monotonicTimeCounter <- getTime MonotonicRaw -- FIXME: Change to MonotonicRaw on Linux
#endif
                       currentTime <- getCurrentTime
                       return $ (show index) ++ "," ++ (show currentTime) ++ "," ++ hostname ++ "," ++ (showTimeSpec monotonicTimeCounter)
          -- Send until everything is done
          sendstr' :: IO String -> IO ()
          sendstr' msg = do omsg <- msg
                            sendstr omsg

          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- SB.sendTo (slSocket syslogh) (B.pack omsg)
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)

closelog :: ServerHandle -> IO ()
closelog syslogh = close (slSocket syslogh)
