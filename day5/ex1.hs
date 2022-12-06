module Ex1 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char

defStr :: Map Int String
defStr = Map.fromList [
    (1,"MFCWTDLB"),
    (2,"LBN"),
    (3,"VLTHCJ"),
    (4,"WJPS"),
    (5,"RLTFCSZ"),
    (6,"ZNHBGDW"),
    (7,"NCGVPSMF"),
    (8,"ZCVFJRQW"),
    (9,"HLMPR")]

convertInput :: [String] -> [[Int]]
convertInput xs = [[firstNum x, secondNum x, thirdNum x] | x <- xs]
    where 
        firstNum x = read (takeWhile (isDigit) $ drop 5 x) :: Int
        secondNum x = read (takeWhile (isDigit) $ dropWhile (\x -> isAlpha x || isSpace x) $ dropWhile (/= 'f') x) :: Int
        thirdNum x = read (takeWhile (isDigit) $ dropWhile (\x -> isAlpha x || isSpace x) $ dropWhile (/= 't') x) :: Int

operation :: Map Int String -> [Int] -> Map Int String
operation mis xs = Map.insert from newFrom $ Map.insert to newTo mis
    where 
        num = head xs
        from = head $ drop 1 xs
        to = last xs
        txt = take num $ mis Map.! from
        newFrom = drop num $ mis Map.! from
        newTo = reverse txt ++ mis Map.! to

operationTotal :: [[Int]] -> Map Int String -> Map Int String
operationTotal xs mis = foldl (\x y -> operation x y) mis xs

getFirsts :: Map Int String -> String
getFirsts mis = foldl (\x y -> x ++ [head y]) "" $ Map.elems mis

compute :: FilePath -> IO String
compute path = do {
    content <- readFile path;
    return $ getFirsts $ operationTotal (convertInput (lines content)) defStr
}

