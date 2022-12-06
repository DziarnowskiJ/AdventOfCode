module Ex2 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

-- OPPONENT
-- A for Rock 
-- B for Paper
-- C for Scissors

-- X Lose
-- Y Draw
-- Z Win

-- POINTS FOR SHAPE
-- 1 for Rock
-- 2 for Paper
-- 3 for Scissors

-- POINTS FOR OUTCOME
-- 0 if you lost
-- 3 if the round was a draw
-- 6 if you won

compute :: FilePath -> IO Int
compute path = do 
    x <- readFile path
    return $ calcScore $ lines x
    
scoreMap :: Map String Int
scoreMap = Map.fromList [
    ("A X", 3), 
    ("B X", 1),
    ("C X", 2),
    ("A Y", 4), 
    ("B Y", 5),
    ("C Y", 6),
    ("A Z", 8), 
    ("B Z", 9),
    ("C Z", 7)]

calcScore :: [String] -> Int
calcScore xs = foldl (+) 0 [scoreMap Map.! val | val <- xs ]