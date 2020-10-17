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


import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import System.IO

data SyslogHandle =
    SyslogHandle {slHandle :: Handle,
                  slProgram :: String}

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
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Mark the socket for keep-alive handling since it may be idle
       -- for long periods of time
       setSocketOption sock KeepAlive 1

       -- Connect to server
       connect sock (addrAddress serveraddr)

       -- Make a Handle out of it for convenience
       h <- socketToHandle sock WriteMode

       -- We're going to set buffering to BlockBuffering and then
       -- explicitly call hFlush after each message, below, so that
       -- messages get logged immediately
       hSetBuffering h (BlockBuffering Nothing)

       -- Save off the socket, program name, and server address in a handle
       return $ SyslogHandle h progname

syslog :: SyslogHandle -> String -> IO ()
syslog syslogh msg =
    do hPutStrLn (slHandle syslogh) sendmsg
       -- Make sure that we send data immediately
       hFlush (slHandle syslogh)
    where sendmsg = (slProgram syslogh) ++ ": " ++ msg

closelog :: SyslogHandle -> IO ()
closelog syslogh = hClose (slHandle syslogh)

main :: IO ()
main = do
    sl <- openlog "localhost" "10514" "tcptest"
    syslog sl "This is my TCP message"
    syslog sl "This is my TCP message again"
    closelog sl
