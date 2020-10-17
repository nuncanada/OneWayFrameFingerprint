module Shared (
  ServerEntry,
  servers,
  getOsMonotonicCounter,
  logLogger
) where

import Data.Csv

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Control.Monad
import Data.Either

import System.Clock as SC
import Data.Time.Clock

timeFormat :: TimeFormat
timeFormat = "%F"

data ServerEntry =
    ServerEntry {
        name :: !String
      , address :: !String
      , port :: !Int
    } deriving (Show)

instance FromNamedRecord ServerEntry where
    parseNamedRecord r = ServerEntry <$> r .: "name" <*> r .: "address" <*> r .: "port"

servers :: IO ([ServerEntry])
servers = do
  csvData <- BL.readFile "servers.txt"
  return (returnServers $ decodeByName csvData)

returnServers :: Either String (Header, V.Vector ServerEntry) -> [ServerEntry]
returnServers (Left _) = []
returnServers (Right (_, v)) = V.toList v

getHostAddress :: SockAddr -> HostAddress
getHostAddress (SockAddrInet _ hostAddress) = hostAddress
getHostAddress _ = error "Nao implementado"

showTimeSpec :: TimeSpec -> String
showTimeSpec   (TimeSpec  (toInteger -> s) (toInteger -> n)) = (show s) ++ "."  ++ (printf "%09d" n)

getOsMonotonicCounter :: IO TimeSpec
getOsMonotonicCounter =
  do
#ifdef mingw32_HOST_OS
    monotonicTimeCounter <- getTime Monotonic
#else
    monotonicTimeCounter <- getTime MonotonicRaw
#endif
    return monotonicTimeCounter

logLogger :: TimedFastLogger -> LogStr -> IO ()
logLogger logger msg = logger (\_ -> msg <> "\n")

timeRotate :: FilePath -> LogType
timeRotate fname = LogFileTimedRotate
               (TimedFileLogSpec fname timeFormat sametime compressFile)
               defaultBufSize
   where
       sametime = (==) --`on` C.takeWhile (/='T')) x)
       compressFile fp = void . forkIO $
           callProcess "gzip" [ fp ]
