module Ex1 where

import Data.List
import System.IO

maxElf :: FilePath -> IO Int
maxElf fp = do
    x <- getLines fp 
    return (maximum $ sumsFromStr $ smartSplit x)


getLines :: FilePath -> IO [String]
getLines path = do 
    contents <- readFile path
    return (lines contents)

-- split list of strings s.t. "" is the divider and is not included in the output list
smartSplit :: [String] -> [[String]]
smartSplit [] = []
smartSplit (x:[]) = [[x]] 
smartSplit (x:xs) 
    | head xs /= "" && x /= "" = [[x] ++ head (smartSplit xs)] ++ tail (smartSplit xs)
    | head xs /= "" && x == ""= [head (smartSplit xs)] ++ tail (smartSplit xs)
    | otherwise = [[x]] ++ smartSplit (tail xs)

-- Return list of sums of strings from a list
sumsFromStr :: [[String]] -> [Int]
sumsFromStr xs = [sumList (ints) | ints <- xs]

-- Return sum of numbers in string form
sumList :: [String] -> Int
sumList [] = 0
sumList (x:xs) = read x + sumList xs
