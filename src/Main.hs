module Main (main) where

import Client (client)
import Server (server)

import Control.Concurrent

cloudServers :: [String]
cloudServers = ["losangeles", "quebec", "saopaulo", "thedalles", "ashburn"]

udpPort :: String
udpPort = "1905"

main :: IO ()
main = do
    _ <- forkIO $ client cloudServers udpPort
    server udpPort
