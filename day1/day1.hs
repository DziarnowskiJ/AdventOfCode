module Day1 where

import Data.List
import System.IO

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- split list of strings s.t. "" is the divider and is not included in the output list
smartSplit :: [String] -> [[String]]
smartSplit [] = []
smartSplit (x:[]) = [[x]] 
smartSplit (x:xs) 
    | head xs /= "" && x /= "" = [[x] ++ head (smartSplit xs)] ++ tail (smartSplit xs)
    | head xs /= "" && x == ""= [head (smartSplit xs)] ++ tail (smartSplit xs)
    | otherwise = [[x]] ++ smartSplit (tail xs)

-- convert Strings in doubly nested List into Ints
strToInt :: [[String]] -> [[Int]]
strToInt xss = [[ read x :: Int | x <- xs] | xs <- xss]

-- converts string input in which there are groups of numbers (each number on separate line)
-- divided by empty line into doubly nested List of Ints
-- eg. converts "XYZ\nPQR\n\nABC" into [[XYZ,PQR],[ABC]]
prepareInput :: String -> [[Int]]
prepareInput s = strToInt $ smartSplit $ lines s

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- returns the highest sum from groups of Ints
maxSum :: [[Int]] -> Int
maxSum xss = maximum $ sumGroups xss

-- sums all Ints that are in the same sublist of input list
sumGroups :: [[Int]] -> [Int]
sumGroups xss = [sum xs | xs <- xss]

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- returns the sum of X number of highest sums from groups of Ints
maxXSum :: Int -> [[Int]] -> Int
maxXSum n xss = sum $ take n $ reverse $ sort $ sumGroups xss

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = maxSum $ prepareInput s
-- answer 1 = 70116

-- solution for part 2
answer2 :: String -> Int
answer2 s = maxXSum 3 $ prepareInput s
-- answer 2 = 206582

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input