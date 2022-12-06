module Ex2 where 

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
    | a1 <= b1 && b1 <= a2 = 1
    | b1 <= a1 && a1 <= b2 = 1
    | otherwise = 0
    where 
        a1 = read (head $ head xs) :: Int
        a2 = read (last $ head xs) :: Int
        b1 = read (head $ last xs) :: Int
        b2 = read (last $ last xs) :: Int

countOverlaps :: [String] -> Int
countOverlaps ss = sum [testOverlap $ splitInput s | s <- ss]

compute :: FilePath -> IO Int
compute path = do {
    content <- readFile path;
    return $ countOverlaps (lines content)
}