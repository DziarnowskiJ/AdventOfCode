module Day3 where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

--------------------------------------------------------------------------------
-- PROCESSING
--------------------------------------------------------------------------------

-- convers Char to Int 
-- st. a = 1, b = 2 ... A = 27, B = 28 ...
convert :: Char -> Int
convert x 
    | isUpper x = ord x - 38
    | otherwise = ord x - 96

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- returns first have of list
-- NOTE: if list has odd number of elements, it will not return the middle element
getHalf :: [a] -> [a]
getHalf xs = take (length xs `div ` 2) xs

-- splits list into two halfs and returns it as tuple
-- NOTE: if list has odd number of elements, middle one will be removed
halfSplit :: [a] -> ([a], [a])
halfSplit s = (getHalf s, getHalf $ reverse s)

-- finds all Chars that are common in BOTH Strings
common :: (String, String) -> Set Char
common (x, y) = Set.intersection (Set.fromList x) (Set.fromList y)

-- returns values of chars (see 'conver' above) of all chars common in both strings
commonVal :: (String, String) -> [Int]
commonVal ss = [convert x | x <- Set.elems $ common ss]

-- sum of Ints from 2D list
sum2D :: [[Int]] -> Int
sum2D xss = sum [sum xs | xs <- xss]

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- splits list into 2D list, where each nested list has X elements
-- NOTE: depending on the size of input list last element of returned list 
--       might have less then X elements
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs = (take i xs) : chunks i (drop i xs)

-- returns a Set of Characters that are present in ALL strings of provided list
commonInList :: [String] -> Set Char
commonInList xs = foldl1 (\x y -> Set.intersection x y) ss
    where ss = [Set.fromList x | x <- xs]

-- returns values of Chars (see 'conver' above) that are common in 
-- in Strings of chunks of size N of the input List
-- where chunk is a List of consecutive elements of input List
commonInChunkVal :: Int -> [String] -> [[Int]]
commonInChunkVal n xs = [map convert (Set.elems x) | x <- map commonInList (chunks n xs)]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = sum2D [commonVal $ halfSplit x | x <- lines s]
-- answer 1 = 7727

-- solution for part 2
answer2 :: String -> Int
answer2 s = sum2D $ commonInChunkVal 3 (lines s)
-- answer 2 = 2609

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input