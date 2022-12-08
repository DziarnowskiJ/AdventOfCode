module Day7 where

import System.IO
import Data.List
import Data.Char
import Data.Text (replace, unpack, pack)

--------------------------------------------------------------------------------------------
-- TREES 
--------------------------------------------------------------------------------------------
data MyTree = File (Int, String) | Branch String [MyTree]
    deriving (Show, Read)

-- return the sum of nodes in the tree
sizeTree :: MyTree -> Int
sizeTree (File x) = fst x
sizeTree (Branch s xs) = foldl (\x y -> x + sizeTree y) 0 xs

-- return the sum of nodes that are smaller than i
sizeTreeLess :: Int -> MyTree -> Int
sizeTreeLess i (File x)
    | z < i = z
    | otherwise = 0
    where z = sizeTree (File x)
sizeTreeLess i (Branch s xs) = foldl (\x y -> x + sizeTreeLess i y) 0 xs

-- return array of Ints with size of each subtree
sizeSubtree :: MyTree -> [Int]
sizeSubtree (File x) = [fst x]
sizeSubtree (Branch s xs) = [foldl (\x y -> x + sizeTree y) 0 xs] ++ (foldl (\x y -> x ++ sizeSubtree y) [] xs)

-- return the sum of Ints from the list that are less than i
sumLess :: Int -> [Int] -> Int
sumLess i [] = 0
sumLess i (x:[])
    | x < i = x
    | otherwise = 0
sumLess i (x:xs)
    | x < i = x + sumLess i xs
    | otherwise = sumLess i xs

-- return sum of nodes that are less than i
sumDirUnder :: Int -> MyTree -> Int
sumDirUnder i t = sumLess i (sizeSubtree t) - sizeTreeLess i t

-- return the 'required size to remove' 
spaceToFree :: Int -> Int -> MyTree -> Int
spaceToFree total required tree = abs(total - sizeTree tree - required)

-- find smallest node that has the size greater than i
fileToRemove :: Int -> MyTree -> Int
fileToRemove i tree = head $ dropWhile (< i) (sort $ sizeSubtree tree)
-- target 1112963

------------------------------------------------------------------------------------------
-- CONVERT INPUT TO TREE

-- Following conversions are required (in specified order)

-- Regex                        --> Change with  
-- \$ ls 			            --> ___                     
-- dir \w+ 			            --> ___                     

-- \n(?=\d+)			        --> \n(File (               
-- (?<=\d+) (?=\w+.*)		    --> ,"                  
-- (?<="\w+.*)\n			    --> ")),\n              

-- \$ cd \.\.			        --> ]),                     
-- \$ cd_				        --> (Branch "               
-- (?<=(\(Branch "[\w/]*))\n	--> " [             

-- \n				            --> ___                         
-- ,\]				            --> ]                           

-- --> Remove last ,
-- --> add ])	^as much as needed = (count \[ - count \])

------------------------------------------------------------------------------------------

-- \$ ls        --> ___ 
process1 :: String -> String
process1 s = 
    unpack $
    replace (pack "$ ls") (pack "") $
    pack s

-- dir \w+      --> ___
process2 :: String -> String
process2 s = 
    unlines $
    filter (\x -> head x /= 'd') $
    filter (not . null) (lines s)

-- \n(?=\d+)            --> \n(File (
-- (?<=\d+) (?=\w+.*)	--> ,"
-- (?<="\w+.*)\n		--> ")),\n
process3 :: String -> String
process3 s = 
    unlines $
    map (\x -> if isDigit (head x) 
        then 
            "(File (" ++ 
            takeWhile (isDigit) x ++
            ",\"" ++
            dropWhile (\x -> isDigit x || isSpace x) x ++
            "\"))," 
        else x) $
    lines s
    

-- \$ cd \.\.		--> ]),
process4 :: String -> String
process4 s = 
    unpack $
    replace (pack "$ cd ..") (pack "]),") $
    pack s

-- \$ cd_		                --> (Branch "
-- (?<=(\(Branch "[\w/]*))\n	--> " [
process5 :: String -> String
process5 s = 
    unlines $
    map (\x -> if (head x) == '$'
        then 
            "(Branch \"" ++ 
            drop 5 x ++
            "\" [" 
        else x) $
    lines s
    

-- \n		--> ___
process6 :: String -> String
process6 s = foldl1 (++) $ lines s

-- ,\]		--> ]
process7 :: String -> String
process7 s = 
    unpack $
    replace (pack ",]") (pack "]") $
    pack s

-- --> Remove last ,
process8 :: String -> String
process8 s = init s

-- --> add ])	^as much as needed = (count \[ - count \])
process9 :: String -> String
process9 s = 
    s ++ 
    foldl1 (++) 
    (take ( (-) 
        (length $ filter (== '[') s)
        (length $ filter (== ']') s)) $ 
    repeat "])")
    
-- convert processed input to tree
inputToTree :: String -> MyTree
inputToTree s = read (process9 $ process8 $ process7 $ 
                        process6 $ process5 $ process4 $ 
                        process3 $ process2 $ process1 s) :: MyTree

----------------------------------------------------------------------------------------
-- COPMUTE ANSWERS
----------------------------------------------------------------------------------------

-- answer for part 1
answer1 :: MyTree -> Int
answer1 tree = sumDirUnder 100000 tree
-- answer 1 = 1792222

-- answer for part 2
answer2 :: MyTree -> Int
answer2 tree = fileToRemove (spaceToFree 70000000 30000000 tree) tree
-- answer 2 = 1112963

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    content <- readFile path
    let tree = inputToTree content
    putStr "Part 1: "
    putStrLn $ show $ answer1 $ tree
    putStr "Part 2: "
    putStrLn $ show $ answer2 $ tree

