module Day8 where

import Data.List
import System.IO

-----------------------------------------------------------------------------------------------------------
-- PART 1
-----------------------------------------------------------------------------------------------------------

-- divide the list into sections such that 
-- [1,2,3] --> [([],1[2,3]),([1],2,[3]),([1,2],3,[])]
sections :: [Int] -> [([Int], Int, [Int])]
sections xs = [(a, (head c), (tail c)) | (a, c) <- zip (inits xs) (tails xs),
                                            length c > 0]

-- checks whether a tree is visible from the border
-- it is visible if there are no trees of equal hight or higher on line of sight
-- eg. 1 4 2 1 8 
-- --> when checking tree size 2
--      - it is not visible (from left is covered by 4 and right by 8)
-- --> when checking tree size 4
--      - it is visible (on the left there are no trees that could hide it)
horizVisPass :: [Int] -> [Bool]
horizVisPass xs = map (\(l,x,r) -> 
                        if (maximum (l ++ [0])) < x || 
                            (maximum (r ++ [0])) < x 
                            then True else False) $ 
                        sections xs

-- return horizPass for the list of lists 
-- (checks are done only horizontally)
horizVisAll :: [[Int]] -> [[Bool]]
horizVisAll xss = [horizVisPass xs | xs <- xss]

-- same as horizVisAll but vertically
vertVisAll :: [[Int]] -> [[Bool]]
vertVisAll xss = transpose $ horizVisAll $ transpose xss

-- assert OR on each location in two lists
-- [[a, b, c],      [[1, 2, 3],     [[a||1, b||2, c||3],
--  [d, e, f],  ||   [4, 5, 6],  =   [d||4, e||5, f||6],
--  [g, h, i]]       [7, 8, 9]]      [g||7, j||8, i||9]]
boolOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
boolOr l r = zipWith (\x y -> zipWith(\z w -> (z || w)) x y ) l r   

-- create 2D list filled with False on the inside and True on the outside border
-- Input list is taken as the dimention parameter
-- eg.
-- [[T,T,T,T,T],
--  [T,F,F,F,T],
--  [T,F,F,F,T],
--  [T,F,F,F,T],
--  [T,T,T,T,T]]
roundBool :: [[Int]] -> [[Bool]]
roundBool xss = 
    -- first row                                
    [take (length $ head xss) $ repeat True] ++ 
    -- middle rows
    (take (length xss -2) $ repeat ([True] ++ (take ((length $ head xss) -2 ) $ repeat False) ++ [True]) ) ++
    -- last row
    [take (length $ head xss) $ repeat True]

-- return map of visible trees 
finBool :: [[Int]] -> [[Bool]]
finBool xss = boolOr (roundBool xss) $ 
        boolOr (horizVisAll xss) (vertVisAll xss)

-- return number of True's that are in the list
countTrue :: [Bool] -> Int
countTrue xs = sum (map (\x -> if x then 1 else 0) xs)

-- countTrue but for 2D list
countAllTrue :: [[Bool]] -> Int
countAllTrue xss = sum [countTrue xs | xs <- xss]

-- solution for part 1 for AdventOfCode2022 DAY 8 
part1 :: [[Int]] -> Int
part1 xss = countAllTrue $ finBool xss

-----------------------------------------------------------------------------------------------------------
-- PART 2
-----------------------------------------------------------------------------------------------------------

-- return list describing how many trees are visible from wach tree
-- tree stops to be visible when it is behind the tree taller or equal than checked from
-- return value is 'visible trees on the left' * 'visible trees on the right'
-- eg. 1 4 2 1 8 
-- --> when checking from tree size 2
--      - to the left 1 tree is visible (size 4)
--      - to the right - 2 trees
horizDistPass :: [Int] -> [Int]
horizDistPass xs = map (\(l,m,r) -> 
    ((if (length $ takeWhile (<m) $ reverse l) == length l then 0 else 1) + (length $ takeWhile (<m) $ reverse l) * 
    ((if ((length $ takeWhile (<m) r) == length r) then 0 else 1) + (length $ takeWhile (<m) r))) )$ 
    sections xs

-- return horizDistPass for the list of lists 
-- (checks are done only horizontally)
horizDistAll :: [[Int]] -> [[Int]]
horizDistAll xss = [horizDistPass xs | xs <- xss]

-- same as horizDistAll but vertically
vertDistAll :: [[Int]] -> [[Int]]
vertDistAll xss = transpose $ horizDistAll $ transpose xss

-- multiply two lists together
-- [[a, b, c],      [[1, 2, 3],     [[a1,b2,c3],
--  [d, e, f],  X    [4, 5, 6],  =   [d4,e5,f6],
--  [g, h, i]]       [7, 8, 9]]      [g7,j8,i9]]
multList :: [[Int]] -> [[Int]] -> [[Int]]
multList xss yss = zipWith (\x y -> zipWith(\z w -> (z * w)) x y ) xss yss

-- return biggest Int from 2D list
maxMax :: [[Int]] -> Int
maxMax xss = maximum [maximum xs | xs <- xss]

-- solution for part 1 for AdventOfCode2022 DAY 8 
part2 :: [[Int]] -> Int
part2 xss = maxMax $ multList (horizDistAll xss) (vertDistAll xss)

-----------------------------------------------------------------------------------------------------------
-- READ INPUT AND DISPLAY ANSWERS
-----------------------------------------------------------------------------------------------------------

-- convert String of numbers as list of Ints
strToInt :: String -> [Int]
strToInt s = read ("[" ++ (intersperse ',' s) ++ "]") :: [Int]

-- convert list of Strings to 2D list of Int's
convertInput :: String -> [[Int]]
convertInput s = [strToInt x | x <- lines s]

-- show solutions
compute :: FilePath -> IO ()
compute path = do
    content <- readFile path
    let input = convertInput content
    putStr "Part 1: "
    putStrLn $ show $ part1 input
    putStr "Part 2: "
    putStrLn $ show $ part2 input

