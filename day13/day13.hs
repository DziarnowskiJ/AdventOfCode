module Day where 
-- :set -i../utilities
import System.IO
import Data.List
import Parser


--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-----------
-- PARSERS
-----------

-- parse signal
signal :: Parser Signal
signal = 
    Single <$> int
    <|> Many <$ char '[' <*> sepBy1 signal (string ",") <* char ']'
    <|> Empty <$ char '[' <* char ']'

-- convert input to list of tuples (Signal, Signal)
pairInput :: [String] -> [Pair]
pairInput [] = []
pairInput s = 
    (\x -> [(head (parseAll signal $ head x), 
            head (parseAll signal $ last x))]) (take 2 s) ++
    (pairInput $ drop 3 s)

-- parse many signals
manySignals :: Parser [Signal]
manySignals = sepBy1 signal (string "\n\n" <|> string "\n") <* string "\n"

-- convert input to list of Signals (empty lines are ignored)
listInput :: String -> [Signal]
listInput s = head $ parseAll manySignals s

----------------
-- SIGNAL STUFF
----------------

-- define signal 
data Signal = Single Int | Many [Signal] | Empty
    deriving (Show)

-- define signal comparison (==)
instance Eq (Signal) where 
    Single m == Single n    = m == n
    Many n == Many m        = n == m
    Empty == Empty          = True
    _ == _                  = False

-- define signal comparison (<=)
instance Ord (Signal) where  
    -- both values are Singles
    Single m <= Single n    = m <= n
    -- both values are Many
    Many m <= Many n        = m <= n
    -- one value is Single, other is Many
    Single m <= Many n      = [Single m] <= n  
    Many m <= Single n      = m <= [Single n]
    -- anything compared to Empty is greater
    Empty <= _              = True
    _ <= Empty              = False

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define Pair as tuple of Signals
type Pair = (Signal, Signal)

-- test whether left signal in a pair is smaller then the right one 
isPairOrdered :: Pair -> Bool
isPairOrdered (l, r) = l <= r

-- returns sum of indecies of which
-- left signal from a pair is smaller than the right one
countOrderedInput :: String -> Int
countOrderedInput s =  
    snd $
    foldl (\(i, s) y -> if y == True then (i + 1, s + i + 1) else (i + 1, s)) 
        (0, 0) (map isPairOrdered $ pairInput $ lines s)

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- sort list of signals in ascending order
sortSignals :: [Signal] -> [Signal]
sortSignals ss = sort ss

-- combine two lists of signals
addSignals :: [Signal] -> [Signal] -> [Signal]
addSignals ss xs = ss ++ xs

-- find first occurance of signals from the input list (and return their index)
findSignals :: [Signal] -> [Signal] -> [Int]
findSignals toFinds ss = map (\x -> 1 + length x) $ [takeWhile (/= s) ss | s <- toFinds]

-- return decoder key
-- it is calculated based om indecies of firstDiv and lastDiv
-- of ordered Signal list
decoderKey :: String -> Int
decoderKey s = 
    (\x -> (head x * last x)) $
    findSignals [firstDiv, lastDiv] $ 
    sortSignals $ 
    addSignals [firstDiv, lastDiv] $ 
    listInput s

-- define signal dividers 
firstDiv :: Signal
firstDiv = Many [Single 2]
lastDiv :: Signal
lastDiv = Many [Single 6]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = countOrderedInput s
-- answer 1 = 6428

-- solution for part 2
answer2 :: String -> Int
answer2 s = decoderKey s
-- answer 2 = 22464

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input