{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage str = let 
