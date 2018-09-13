{-# LANGUAGE ViewPatterns #-}

module Common where

import System.Clock
import Text.Printf

showTimeSpec :: TimeSpec -> String
showTimeSpec   (TimeSpec  (toInteger -> s) (toInteger -> n)) = (show s) ++ "."  ++ (printf "%09d" n)
