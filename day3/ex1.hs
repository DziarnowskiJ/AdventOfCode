module Ex1 where

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

convert :: Char -> Int
convert x 
    | isUpper x = ord x - 38
    | otherwise = ord x - 96

getHalf :: String -> String
getHalf s = take (length s `div ` 2) s

halfSplit :: String -> (String, String)
halfSplit s = (getHalf s, getHalf $ reverse s)

common :: (String, String) -> Set Char
common (x, y) = Set.intersection (Set.fromList x) (Set.fromList y)

getCommonVal :: String -> Int
getCommonVal s = convert $ Set.elemAt 0 $ common $ halfSplit s

getCommonValSum :: [String] -> Int
getCommonValSum xs = sum [getCommonVal x | x <- xs]

compute :: FilePath -> IO Int
compute path = do 
    content <- readFile path;
    return $ getCommonValSum (lines content)
