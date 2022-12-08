module Day5 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char

----------------------------------------------------------------------------------------------
-- INPUT PROCESSING
----------------------------------------------------------------------------------------------

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

convertInput :: String -> [(Int, Int, Int)]
convertInput s = [(firstNum x, secondNum x, thirdNum x) | x <- lines s]
    where 
        firstNum x = read (takeWhile (isDigit) $ drop 5 x) :: Int
        secondNum x = read (takeWhile (isDigit) $ dropWhile (\x -> isAlpha x || isSpace x) $ dropWhile (/= 'f') x) :: Int
        thirdNum x = read (takeWhile (isDigit) $ dropWhile (\x -> isAlpha x || isSpace x) $ dropWhile (/= 't') x) :: Int

----------------------------------------------------------------------------------------------
-- PART 1
----------------------------------------------------------------------------------------------

-- perform operation 'move X from Y to Z' defined as tuple of 3 Int (X,Y,Z)
-- that takes first X characters from Y and adds them in front of Z's string in reverse order
operation :: Map Int String -> (Int, Int, Int) -> Map Int String
operation mis xs = Map.insert from newFrom $ Map.insert to newTo mis
    where 
        num = (\(x, _, _) -> x) xs
        from = (\(_, x, _) -> x) xs
        to = (\(_, _, x) -> x) xs
        txt = take num $ mis Map.! from
        newFrom = drop num $ mis Map.! from
        newTo = reverse txt ++ mis Map.! to

-- performs operation above to list of tuples [(X,Y,Z)]
operationTotal :: Map Int String -> [(Int, Int, Int)] ->  Map Int String
operationTotal mis xs = foldl (\x y -> operation x y) mis xs

-- returns string of first characters of values from Map Int String
getFirsts :: Map Int String -> String
getFirsts mis = foldl (\x y -> x ++ [head y]) "" $ Map.elems mis

----------------------------------------------------------------------------------------------
-- PART 2
----------------------------------------------------------------------------------------------

-- perform operation 'move X from Y to Z' 
-- that takes first X characters from Y and adds them in front of Z's string
operation2 :: Map Int String -> (Int, Int, Int) -> Map Int String
operation2 mis xs = Map.insert from newFrom $ Map.insert to newTo mis
    where 
        num = (\(x, _, _) -> x) xs
        from = (\(_, x, _) -> x) xs
        to = (\(_, _, x) -> x) xs
        txt = take num $ mis Map.! from
        newFrom = drop num $ mis Map.! from
        newTo = txt ++ mis Map.! to

-- performs operation2 above to list of tuples [(X,Y,Z)]
operation2Total :: Map Int String -> [(Int, Int, Int)] ->  Map Int String
operation2Total mis xs = foldl (\x y -> operation2 x y) mis xs

------------------------------------------------------------------
-- SHOW ANSWERS
------------------------------------------------------------------

-- solution for part 1
answer1 :: String -> String
answer1 s = getFirsts $ operationTotal defStr (convertInput s)
-- answer 1 = TGWSMRBPN

-- solution for part 2
answer2 :: String -> String
answer2 s = getFirsts $ operation2Total defStr (convertInput s)
-- answer 2 = TZLTLWRNF

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input