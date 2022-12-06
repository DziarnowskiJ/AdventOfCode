module Ex2 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

diffX :: Int -> String -> Int
diffX n xs
    | n == Set.size (Set.fromList $ take n xs) = n
    | otherwise = 1 + (diffX n $ tail xs)

compute :: FilePath -> IO Int
compute path = do {
    content <- readFile path;
    return $ diffX 14 content
}