module Day4 where 

import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- splits input into substrings based on X and returns it as list of those substrings
-- NOTE: X is removed from all substrings, and list does not contain empty strings
splitOn :: Char -> String -> [String]
splitOn x "" = []
splitOn x (s : ss) 
    | s == x = splitOn x ss
    | otherwise = takeWhile (/= x) (s:ss) : (splitOn x (dropWhile (/= x) (s:ss)))

-- splits input in form "W-X,Y-Z" into 4 Int tuple (W,X,Y,Z)
splitInput :: String -> (Int, Int, Int, Int)
splitInput s = (\[[w, x], [y, z]] -> 
                    (read w :: Int, read x :: Int, read y :: Int, read z :: Int)) 
                [splitOn '-' x | x <- splitOn ',' s]
    
--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- asseses overlap of (W,X,Y,Z) 
-- st. ALL values in range W-X must be between Y-Z or vice versa
-- if overlap happens function returns 1, otherwise 0
testOverlap :: (Int, Int, Int, Int) -> Int
testOverlap xs 
    | Set.isSubsetOf set1 set2 || Set.isSubsetOf set2 set1 = 1
    | otherwise = 0
    where 
        set1 = Set.fromList [(\(x, _, _, _) -> x) xs .. (\(_, x, _, _) -> x) xs]
        set2 = Set.fromList [(\(_, _, x, _) -> x) xs .. (\(_, _, _, x) -> x) xs]

-- counts number of overlaps according to the rule above
countOverlaps :: [String] -> Int
countOverlaps ss = sum [testOverlap $ splitInput s | s <- ss]

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- asseses overlap of (W,X,Y,Z)
-- st. SOME values in range W-X must be between Y-Z or vice versa
-- if overlap happens function returns 1, otherwise 0
testOverlap2 :: (Int, Int, Int, Int) -> Int
testOverlap2 xs 
    | a1 <= b1 && b1 <= a2 = 1
    | b1 <= a1 && a1 <= b2 = 1
    | otherwise = 0
    where 
        a1 = (\(x, _, _, _) -> x) xs
        a2 = (\(_, x, _, _) -> x) xs
        b1 = (\(_, _, x, _) -> x) xs
        b2 = (\(_, _, _, x) -> x) xs

-- counts number of overlaps according to the rule above
countOverlaps2 :: [String] -> Int
countOverlaps2 ss = sum [testOverlap2 $ splitInput s | s <- ss]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- solution for part 1
answer1 :: String -> Int
answer1 s = countOverlaps $ lines s
-- answer 1 = 475

-- solution for part 2
answer2 :: String -> Int
answer2 s = countOverlaps2 $ lines s
-- answer 2 = 825

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input