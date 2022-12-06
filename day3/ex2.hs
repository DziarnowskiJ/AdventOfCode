module Ex2 where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

convert :: Char -> Int
convert x 
    | isUpper x = ord x - 38
    | otherwise = ord x - 96

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs = (take i xs) : chunks i (drop i xs)

commonInSet :: [String] -> Set Char
commonInSet xs = foldl1 (\x y -> Set.intersection x y) ss
    where ss = [Set.fromList x | x <- xs]

commonInAll :: [String] -> [Set Char]
commonInAll xs = map commonInSet (chunks 3 xs)

commonValSum :: [Set Char] -> Int
commonValSum xs = sum [convert $ Set.elemAt 0 x | x <- xs]

compute :: FilePath -> IO Int
compute path = do 
    content <- readFile path;
    return $ commonValSum $ commonInAll (lines content)