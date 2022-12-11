module Day11 where 

import System.IO
import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

{-
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
-}

-- worry function parser
worryFunc :: Parser WorryFunc
worryFunc = Mul <$ string "new = old * " <*> int    -- new = old *
    <|> Add <$ string "new = old + " <*> int        -- new = old +
    <|> Pow <$ string "new = old * old"             -- new = old * old

-- throw location parser
throwLoc :: Parser (Int, Int)
throwLoc = (\x y -> (x,y)) <$ string "If true: throw to monkey " <*> int    -- throw to when True
            <* string "\n    If false: throw to monkey " <*> int            -- throw to when False

-- monkey parser
monkey :: Parser Monkey
monkey = (\id inventory worryFunc dividerTest throwLocations -> 
                    Monkey id inventory worryFunc dividerTest throwLocations 0) 
    <$ string "Monkey " <*> int                                     -- id
    <* string ":\n  Starting items: " <*> sepBy1 int (string ", ")  -- inventory
    <* string "\n  Operation: " <*> worryFunc                       -- worryWorryFunc
    <* string "\n  Test: divisible by " <*> int                     -- dividerTest
    <* string "\n    " <*> throwLoc                                 -- throwLocations
    
-- many monkeys parser
manyMonkeys :: Parser [Monkey]
manyMonkeys = sepBy1 monkey (string "\n\n")

-- return list of monkeys from string
convertInput :: String -> [Monkey]
convertInput s = head $ parseAll manyMonkeys s

-- define MonkeyMap type
-- where monkey's ID is the key, and monkey is the value
type MonkeyMap = Map Int Monkey

-- create Map of monkeys based on parsed string
monkeyMap :: String -> MonkeyMap
monkeyMap s = Map.fromList $ [ (getId m, m) | m <- convertInput s]

pt :: IO ()
pt = do
    input <- readFile "input_small.txt"
    putStrLn $ show $ monkeyMap input

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

----------------
-- MONKEY STUFF
----------------

-- define monkey
--            Monkey ID  Inventory  worryFunction divTest throwLocations  inspectionCount
data Monkey = Monkey Int [Int]      WorryFunc     Int     (Int, Int)      Int
    deriving Show

-- return activity of monkey
getInspCount :: Monkey -> Int
getInspCount (Monkey _ _ _ _ _ insCount) = insCount

-- return monkey's ID
getId :: Monkey -> Int
getId (Monkey id _ _ _ _ _) = id

-- return monkey's inventory
getInventory :: Monkey -> [Int]
getInventory (Monkey _ inventory _ _ _ _) = inventory

-- return divider test value
getDivTest :: Monkey -> Int
getDivTest (Monkey _ _ _ divTest _ _) = divTest

-- add object to a monkey's inventory
addObject :: Monkey -> Int -> Monkey
addObject (Monkey id inventory worryFunc divTest throwLoc insCount) o = 
    (Monkey id (inventory ++ [o]) worryFunc divTest throwLoc insCount)


------------------------
-- WORRY FUNCTION STUFF
------------------------

-- define WorryFunc (possible types of worry function)
data WorryFunc 
    = Mul Int
    | Add Int
    | Pow 
    deriving Show

-- adjust number based on the WorryFunc
doWorryFunc :: WorryFunc -> Int -> Int
doWorryFunc (Mul n) x = x * n
doWorryFunc (Add n) x = x + n
doWorryFunc (Pow) x = x * x

------------------------
-- FUNCTIONS FOR PART 1
------------------------

-- determine the object the monkey will throw and to whom
-- this also updates the monkey's 'inventory'
process :: Monkey -> (Monkey, (Int, Int))
process (Monkey id inventory worryFunc divTest throwLoc insCount) =
    ((Monkey id (tail inventory) worryFunc divTest throwLoc (insCount + 1)), (throwTo, newObject))
    where 
        throwTo = if (newObject `mod` divTest) == 0 then fst throwLoc else snd throwLoc
        newObject = (doWorryFunc worryFunc $ head inventory) `div` 3

-- make one play and update monkey tree
-- ie. one monkey throws one object to another monkey
play :: MonkeyMap -> Int -> MonkeyMap
play ms x = (\mm (m, o) -> 
                Map.insert (fst o) 
                            (addObject (ms Map.! fst o) $ snd o) 
                            (Map.insert x m ms) ) 
                ms (process $ ms Map.! x)

-- make monkey play with all inventory in inventory
-- ie. do 'play ms x' while monkey has something in its inventory
fullPlay :: MonkeyMap -> Int -> MonkeyMap
fullPlay ms x = if size > 0 then fullPlay (play ms x) x else ms
    where size = length $ getInventory $ ms Map.! x

-- round (each monkey makes a fullPlay)
roundPlay :: MonkeyMap -> Int -> MonkeyMap
roundPlay mm 0 = fullPlay mm 0
roundPlay mm x = fullPlay ( roundPlay mm (x-1) ) x

-- perform roundPlay X times
roundPlayX :: MonkeyMap -> Int -> MonkeyMap
roundPlayX mm x = last $ take (x + 1) $ iterate (\m -> roundPlay m (Map.size mm -1)) mm 

-- return activity of all monkeys
getActivities :: MonkeyMap -> [Int]
getActivities mm = reverse $ sort $ map getInspCount (Map.elems mm)

-- --------------------------------------------------------------------------------
-- -- PART 2
-- --------------------------------------------------------------------------------

prodOfDiv :: MonkeyMap -> Int
prodOfDiv mm = foldl (\x y -> x * y) 1 $ map (\(x, y) -> getDivTest y) $ Map.toList mm

-- determine the object the monkey will throw and to whom
-- this also updates the monkey's 'inventory'
-- the difference with 'process' is that worry value is not divided by 3 this time
-- however, to keep the worry value low, mod function is applied
process2 :: Int -> Monkey -> (Monkey, (Int, Int))
process2 prodDiv (Monkey id inventory worryFunc divTest throwLoc insCount) =
    ((Monkey id (tail inventory) worryFunc divTest throwLoc (insCount + 1)), (throwTo, newObject))
    where 
        throwTo = if (newObject `mod` divTest) == 0 then fst throwLoc else snd throwLoc
        newObject = (doWorryFunc worryFunc $ head inventory) `mod` prodDiv

-- make one play and update monkey tree
-- ie. one monkey throws one object to another monkey
-- the difference with 'play' is that it uses 'process2' insted of 'process' 
-- which requires additional parameter in form of product of dividers (monkeys' divTest)
play2 :: MonkeyMap -> Int -> MonkeyMap
play2 mm x = (\mm (m, o) -> 
                Map.insert (fst o) 
                            (addObject (mm Map.! fst o) $ snd o) 
                            (Map.insert x m mm) 
            ) mm (process2 (prodOfDiv mm) (mm Map.! x))

-- make monkey play with all inventory in inventory
-- ie. do 'play mm x' while monkey has something in its inventory
-- only difference with 'fullPlay' is that it uses 'play2' instead of 'play'
-- this is required to avoid having large worry values
fullPlay2 :: MonkeyMap -> Int -> MonkeyMap
fullPlay2 mm x = if size > 0 then fullPlay2 (play2 mm x) x else mm
    where size = length $ getInventory $ mm Map.! x

-- round (each monkey makes a fullPlay)
-- only difference with 'roundPlay' is that it uses 'fullPlay2' instead of 'fullPlay'
-- this is required to avoid having large worry values
roundPlay2 :: MonkeyMap -> Int -> MonkeyMap
roundPlay2 mm 0 = fullPlay2 mm 0
roundPlay2 mm x = fullPlay2 ( roundPlay2 mm (x-1) ) x

-- perform roundPlay X times
-- only difference with 'roundPlayX' is that it uses 'roundPlay2' instead of 'roundPlay'
-- this is required to avoid having large worry values
roundPlayX2 :: MonkeyMap -> Int -> MonkeyMap
roundPlayX2 mm x = last $ take (x + 1) $ iterate (\m -> roundPlay2 m (Map.size mm -1)) mm 

-- --------------------------------------------------------------------------------
-- -- SHOW ANSWERS
-- --------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = foldl (*) 1 $ take 2 $ getActivities $ roundPlayX (monkeyMap s) 20
-- answer 1 = 95472

-- solution for part 2
answer2 :: String -> Int
answer2 s = foldl (*) 1 $ take 2 $ getActivities $ roundPlayX2 (monkeyMap s) 10000
-- answer 2 = 17926061332

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input