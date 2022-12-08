module Day2 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

------------------------------------------------------------------
-- PART 1
------------------------------------------------------------------

-- OPPONENT
-- A for Rock 
-- B for Paper
-- C for Scissors

-- ME
-- X for Rock
-- Y for Paper
-- Z for Scissors

-- POINTS FOR SHAPE
-- 1 for Rock
-- 2 for Paper
-- 3 for Scissors

-- POINTS FOR OUTCOME
-- 0 if you lost
-- 3 if the round was a draw
-- 6 if you won
    
scoreMap :: Map String Int
scoreMap = Map.fromList [
    ("A X", 4),
    ("B X", 1),
    ("C X", 7),
    ("A Y", 8), 
    ("B Y", 5),
    ("C Y", 2),
    ("A Z", 3), 
    ("B Z", 9),
    ("C Z", 6)]

-- calculate cumulative score based on the socreMap provided as input
calcScore :: Map String Int -> [String] -> Int
calcScore msi xs = foldl (+) 0 [msi Map.! val | val <- xs ]

------------------------------------------------------------------
-- PART 2
------------------------------------------------------------------

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

scoreMap2 :: Map String Int
scoreMap2 = Map.fromList [
    ("A X", 3), 
    ("B X", 1),
    ("C X", 2),
    ("A Y", 4), 
    ("B Y", 5),
    ("C Y", 6),
    ("A Z", 8), 
    ("B Z", 9),
    ("C Z", 7)]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = calcScore scoreMap $ lines s
-- answer 1 = 11150

-- solution for part 2
answer2 :: String -> Int
answer2 s = calcScore scoreMap2 $ lines s
-- answer 2 = 8295

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input