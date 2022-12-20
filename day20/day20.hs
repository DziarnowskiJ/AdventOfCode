module Day20 where 

import System.IO
import Data.List

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

convertInput :: String -> [Pair]
convertInput s = [(read numb :: Int, False) | numb <- lines s]

convertInput2 :: String -> [IndexPair]
convertInput2 s = zip [1..] [(val * 811589153, bool) | (val, bool) <- convertInput s]

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define Pair as tuple (Number, wasMoved)
type Pair = (Int, Bool)

-- define IndexPair as tuple (Index, Pair)
type IndexPair = (Int, Pair)

-- split the list of pairs 
-- st. ([moved pairs], [next pair to move], [more pairs to move])
split :: [Pair] -> ([Pair], [Pair], [Pair])
split ps = 
    (\(x, y) -> 
        if null y 
            -- no more numbers to move
            then (x, [], [])
            else if length y == 1
                -- one last item to move
                then (x, y, [])
                -- many numbers to move
                else (x, [head y], tail y)) $ 
    span (\(num, bool) -> bool == True) ps

-- split list on index
splitOn :: Int -> [a] -> ([a], [a])
splitOn ind ls = (take ind ls, drop ind ls)

-- from the tuple of (Left list, X, Right list)
-- move X (which must be a list [Pair] of length 1)
-- number of spaces to the left or right 
-- (this number is determined) by Number from a pair (Num, bool)
move :: ([Pair], [Pair], [Pair]) -> [Pair]
-- no more numbers to move
move (xs, [], []) = xs
-- many numbers to move
move (xs, mid@((y_val, y_bool):empty), ys) = 
    if y_val == 0
        -- y_val == 0 (no move)
        then (xs ++ [(y_val, True)] ++ ys)
        -- y_val /= 0 (move required)
        else if y_move > 0
            -- move right
            then if y_move <= length ys
                -- list is long enough
                then (\(l, r) -> xs ++ l ++ [(y_val, True)] ++ r) $ splitOn y_move ys
                -- list is too short (cycle required)
                else (\(l, r) -> l ++ [(y_val, True)] ++ r ++ ys) $ splitOn (y_move - length ys) xs
            -- move left
            else if (abs y_move < length xs)
                -- list is long enough
                then (\(l, r) -> l ++ [(y_val, True)] ++ r ++ ys) $ splitOn (y_move + length xs) xs
                -- list is too short (cycle required)
                else (\(l, r) -> xs ++ l ++ [(y_val, True)] ++ r) $ splitOn (length ys + (length xs + y_move)) ys
    where 
        y_move = if y_val > 0 
            then y_val `mod` (length xs + length ys)
            else  y_val `mod` (length xs + length ys) - (length xs + length ys)

-- decode the list of Pairs
decode :: [Pair] -> [Pair]
decode ps = 
    if ps == decoded 
        then decoded
        else decode decoded
    where decoded = move $ split ps

-- return index at which there is a pair (0, True)
zeroIndex :: [Pair] -> Int
zeroIndex ps = length $ takeWhile (/= (0, True)) ps

-- return Num from Pair (Num, Bool), that is at soecified index 
getNumAtIndex :: Int -> [Pair] -> Int
getNumAtIndex ind ps = (\(val, bool) -> val) $ head $ drop (ind `mod` (length ps)) ps

-- return 3 numbers that the challenge requires
-- (those located 1000, 2000 and 3000 spaces after 0 )
getAnsw :: [Pair] -> (Int, Int, Int)
getAnsw ps = 
    (getNumAtIndex (zerInd + 1000) ps,
    getNumAtIndex (zerInd + 2000) ps,
    getNumAtIndex (zerInd + 3000) ps)
    where zerInd = zeroIndex ps

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- split the list of IndexPairs 
-- st. ([moved pairs], [next pair to move], [more pairs to move])
-- next pair to move will be the one with SMALLEST index and bool = False
splitIndexed :: [IndexPair] -> ([IndexPair], [IndexPair], [IndexPair])
splitIndexed ips = 
    if null smlFalse 
        -- no more numbers to move
        then (ips, [], [])
        -- some numbers to move
        else (\(l, r) -> (l, [head smlFalse], (drop 1 r))) $ span (/= head smlFalse) ips
    where 
        smlFalse = dropWhile (\(ind, (num, bool)) -> bool == True) (sort ips)

-- works like 'move' defined above, but for the IndexPair instead of Pair
moveIndexed :: ([IndexPair], [IndexPair], [IndexPair]) -> [IndexPair]
-- no more numbers to move
moveIndexed (xs, [], []) = xs
-- many numbers to move
moveIndexed (xs, mid@(ind, (y_val, y_bool)):empty, ys) = 
    if y_val == 0
        -- y_val == 0 (no move)
        then (xs ++ [(ind, (y_val, True))] ++ ys)
        -- y_val /= 0 (move required)
        else if y_move > 0
            -- move right
            then if y_move <= length ys
                -- list is long enough
                then (\(l, r) -> xs ++ l ++ [(ind, (y_val, True))] ++ r) $ splitOn y_move ys
                -- list is too short (cycle required)
                else (\(l, r) -> l ++ [(ind, (y_val, True))] ++ r ++ ys) $ splitOn (y_move - length ys) xs
            -- move left
            else if (abs y_move < length xs)
                -- list is long enough
                then (\(l, r) -> l ++ [(ind, (y_val, True))] ++ r ++ ys) $ splitOn (y_move + length xs) xs
                -- list is too short (cycle required)
                else (\(l, r) -> xs ++ l ++ [(ind, (y_val, True))] ++ r) $ splitOn (length ys + (length xs + y_move)) ys
    where 
        y_move = if y_val > 0 
            then y_val `mod` (length xs + length ys)
            else  y_val `mod` (length xs + length ys) - (length xs + length ys)

-- works like 'decode' defined above, but for the IndexPair instead of Pair
decodeIndexed :: [IndexPair] -> [IndexPair]
decodeIndexed ps = 
    if ps == decoded 
        then decoded
        else decodeIndexed decoded
    where decoded = moveIndexed $ splitIndexed ps

-- perform multiple decodeIndexed operations on list of IndexPair
multiDecodeIndexed :: Int -> [IndexPair] -> [IndexPair]
multiDecodeIndexed 1 ips = decodeIndexed ips
multiDecodeIndexed n ips = decodeIndexed $ swapBool $ multiDecodeIndexed (n-1) ips

-- inverse the value of Bool from IndexPair (Ind, (Num, Bool))
swapBool :: [IndexPair] -> [IndexPair]
swapBool ips = [(ind, (val, not bool)) | (ind, (val, bool)) <- ips]

-- convert list of IndexPair to list of Pair
-- by removing the Ind from (Ind, (Num, Bool))
stripIndex :: [IndexPair] -> [Pair]
stripIndex ips = [(val, bool) |  (ind, (val, bool)) <- ips]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = 
    (\(x, y, z) -> x + y + z) $ getAnsw $ decode $ convertInput s
-- answer 1 = 13883

-- solution for part 2
answer2 :: String -> Int
answer2 s = 
    (\(x, y, z) -> x + y + z) $ getAnsw $ stripIndex $ multiDecodeIndexed 10 (convertInput2 s)
-- answer 2 = 19185967576920

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input