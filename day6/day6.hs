module Day6 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------------
-- PART 1
------------------------------------------------------------------

-- return point at which first substring composed of 4 different characters ends
diffFour :: String -> Int
diffFour (x:y:z:w:rs) 
    | x /= y && x /= z && x /= w &&
        y /= z && y /= w &&
        z /= w = 4
    | otherwise = 1 + diffFour (y:z:w:rs)

------------------------------------------------------------------
-- PART 2
------------------------------------------------------------------

-- return point at which first substring composed of X different characters ends
diffX :: Int -> String -> Int
diffX n xs
    | n == Set.size (Set.fromList $ take n xs) = n
    | otherwise = 1 + (diffX n $ tail xs)

------------------------------------------------------------------
-- SHOW ANSWERS
------------------------------------------------------------------

-- solution for part 1
answer1 :: String -> Int
answer1 s = diffFour s
-- answer 1 = 1175

-- solution for part 2
answer2 :: String -> Int
answer2 s = diffX 14 s
-- answer 2 = 3217

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input