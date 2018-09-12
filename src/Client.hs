{-# LANGUAGE OverloadedStrings #-}

module Client (main) where

import Data.Bits
import Data.List
import qualified Data.ByteString.Char8 as B

import Network.Socket
import qualified Network.Socket.ByteString as SB
import qualified Network.HostName as NH

data SyslogHandle =
    SyslogHandle {slSocket :: Socket,
                  slProgram :: String,
                  slAddress :: SockAddr}

main :: IO ()
main =
  do
    h <- openlog "localhost" "1905" "testprog"
    sendTime h "This is my message"
    closelog h

openlog :: HostName             -- ^ Remote hostname, or localhost
        -> String               -- ^ Port number or name; 514 is default
        -> String               -- ^ Name to log under
        -> IO SyslogHandle      -- ^ Handle to use for logging
openlog hostname port progname =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle sock progname (addrAddress serveraddr)

sendTime :: SyslogHandle -> String -> IO ()
sendTime syslogh msg =
    sendstr' sendmsg
    where sendmsg = do hostname <- NH.getHostName
                       (slProgram syslogh) ++ "-" ++ hostname ++ "-" ++ msg
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
