module Ex1 where 

import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

splitOn :: Char -> String -> [String]
splitOn x "" = []
splitOn x (s : ss) 
    | s == x = splitOn x ss
    | otherwise = takeWhile (/= x) (s:ss) : (splitOn x (dropWhile (/= x) (s:ss)))

splitInput :: String -> [[String]]
splitInput s = [splitOn '-' x | x <- splitOn ',' s]

testOverlap :: [[String]] -> Int
testOverlap xs 
    | Set.isSubsetOf set1 set2 || Set.isSubsetOf set2 set1 = 1
    | otherwise = 0
    where 
        set1 = Set.fromList [read (head $ head xs) :: Int .. read (last $ head xs) :: Int]
        set2 = Set.fromList [read (head $ last xs) :: Int .. read (last $ last xs) :: Int]

countOverlaps :: [String] -> Int
countOverlaps ss = sum [testOverlap $ splitInput s | s <- ss]

compute :: FilePath -> IO Int
compute path = do {
    content <- readFile path;
    return $ countOverlaps (lines content)
}