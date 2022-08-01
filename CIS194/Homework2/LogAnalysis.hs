{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage str = let wordsList = words str in
            case wordsList of 
                ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
                ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
                ("E":num:ts:msg) -> LogMessage (Error (read num :: Int)) (read ts :: Int) (unwords msg)
                _ -> Unknown (unwords wordsList)
