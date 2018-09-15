{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Client (main) where

import Common

import Data.Bits
import Data.List
import qualified Data.ByteString.Char8 as B

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

import System.Clock as SC

import Control.Concurrent

import System.Environment
import System.Exit

data SyslogHandle =
    SyslogHandle {slSocket :: Socket,
                  slAddress :: SockAddr}

main :: IO ()
main =
  do
    args <- getArgs
    let hostname = args !! 0
    main' hostname

main' :: String -> IO ()
main' hostname =
  do
    h <- openlog hostname "1905"
    sendTimeLoop h
    closelog h

sendTimeLoop h = do
  threadDelay (100 * 1000)
  sendTime h
  sendTimeLoop h

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, and server address in a handle
       return $ SyslogHandle sock (addrAddress serveraddr)

sendTime :: SyslogHandle -> IO ()
sendTime syslogh =
    sendstr' sendmsg
    where sendmsg = do hostname <- NH.getHostName
#ifdef mingw32_HOST_OS
                       currentTime <- getTime Monotonic -- FIXME: Change to MonotonicRaw on Linux
#else
                       currentTime <- getTime MonotonicRaw -- FIXME: Change to MonotonicRaw on Linux
#endif
                       return $ hostname ++ "," ++ (showTimeSpec currentTime)
          -- Send until everything is done
          sendstr' :: IO String -> IO ()
          sendstr' msg = do omsg <- msg
                            sendstr omsg

          sendstr :: String -> IO ()
          sendstr [] = return ()
          sendstr omsg = do sent <- SB.sendTo (slSocket syslogh) (B.pack omsg)
                                    (slAddress syslogh)
                            sendstr (genericDrop sent omsg)

closelog :: SyslogHandle -> IO ()
closelog syslogh = close (slSocket syslogh)
