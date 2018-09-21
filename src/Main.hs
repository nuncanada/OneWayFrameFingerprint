module Main (main) where

import Client (client)
import Server (server)

import System.IO
import System.Exit
import Data.Char (toLower)

import Control.Concurrent
import Control.Monad

cloudServers :: [String]
cloudServers = ["losangeles", "quebec", "saopaulo", "thedalles", "ashburn"]

udpPort :: String
udpPort = "1905"

main :: IO ()
main = do
    _ <- forkIO $ server udpPort
    _ <- forkIO $ client cloudServers udpPort
    exitOnQ

exitOnQ :: IO ()
exitOnQ = do
    hSetBuffering stdin NoBuffering
    c <- getChar
    when (toLower c /= 'q') exitOnQ
    exitSuccess  -- or "exitWith" and some ExitCode value, use hoogle.
