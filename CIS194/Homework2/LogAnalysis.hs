{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
-- import Data.List

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage str = let wordsList = words str in
            case wordsList of 
                ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
                ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
                ("E":num:ts:msg) -> LogMessage (Error (read num :: Int)) (read ts :: Int) (unwords msg)
                _ -> Unknown (unwords wordsList)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert lgMsg Leaf = Node Leaf lgMsg Leaf
insert (lgMsg@(LogMessage _ ts1 _)) (msgTree@(Node msgT1 lgMsgTree@(LogMessage _ ts2 _) msgT2)) 
                | ts1 > ts2 = Node msgTree lgMsg Leaf 
                | otherwise = Node Leaf lgMsg msgTree
                    
-- Exercise 3
build :: [LogMessage] -> MessageTree 
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder msgTree@(Node msgT1 lgMsg msgT2) = [lgMsg] ++ (inOrder msgT1) ++ (inOrder msgT2)

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (x@(LogMessage msgType _ strMsg):xs) 
                    | msgType == Error num = checkNum msgType 
                    | otherwise = whatWentWrong xs 
                    where checkNum msgType@(Error num) | num >= 50 = [strMsg] ++ whatWentWrong xs 
                                                       | otherwise = whatWentWrong xs 