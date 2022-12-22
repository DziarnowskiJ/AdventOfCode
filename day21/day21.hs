module Day21 where 

import System.IO
import Parser
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Typeable
-- import Data.Set (Set)
-- import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- parse monkey and its 'node'
monkeyFull :: Parser (MonkeyName, Monkey)
monkeyFull = 
    (\x y -> (x, y)) <$> 
    some letter <*
    string ": " <*>
    monkey 

-- parse Monkey
monkey :: Parser Monkey
monkey = 
    (MonkeyInt <$> int) <|> 
    (MonkeyOp <$> some letter <* space <*> op <* space <*> some letter)

-- parse operation
op :: Parser Op
op = 
    Add <$ char '+' <|>
    Min <$ char '-' <|>
    Mul <$ char '*' <|>
    Div <$ char '/'

-- parse MonkeyMap
monkeyMap :: Parser MonkeyMap
monkeyMap = Map.fromList <$> sepBy1 monkeyFull (char '\n')

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define 4 operation types
data Op = Add | Min | Mul | Div
    deriving (Show, Eq)

-- define MonkeyName as String
type MonkeyName = String

-- define Monkey
-- which in combination with MonkeyMap is basically a tree
data Monkey = MonkeyInt Int | MonkeyOp MonkeyName Op MonkeyName
    deriving (Show, Typeable)

-- define MonkeyMap
-- which maps monkey name to it's node 
type MonkeyMap = Map MonkeyName Monkey

-- determine the number the monkey will shout
-- (perform recursive operations of addition, subtraction, multiplication and division) 
eval :: MonkeyName -> MonkeyMap -> Int
eval m mMap = val_map ! m
    where 
        val_map = Map.map eval_monkey mMap
        eval_monkey (MonkeyInt x) = x
        eval_monkey (MonkeyOp m1 oper m2) = eval_op oper (eval_monkey $ mMap ! m1) (eval_monkey $ mMap ! m2)

-- perform specified operation
eval_op :: Op -> Int -> Int -> Int
eval_op Add m n = m + n 
eval_op Min m n = m - n 
eval_op Mul m n = m * n 
eval_op Div m n = m `div` n     

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- define HumnMap holding information whether humn is in the subtree of each monkey
type HumnMap = Map MonkeyName Bool

-- make HumnMap holding information whether humn is in the subtree of each monkey
mkHumnMap :: MonkeyMap -> HumnMap
mkHumnMap mMap = Map.map (\x -> leadToHumn x mMap) mMap
        
-- test whether humn is in the monkey's subtree 
leadToHumn :: Monkey -> MonkeyMap -> Bool
leadToHumn (MonkeyInt _) _ = False
leadToHumn (MonkeyOp "humn" _ _) _ = True
leadToHumn (MonkeyOp _ _ "humn") _ = True
leadToHumn (MonkeyOp mName1 _ mName2) mMap = ((leadToHumn (mMap ! mName1) mMap) || (leadToHumn (mMap ! mName2) mMap))

-- determin what value must 'humn' have 
-- st. both subtrees of a monkey have the same value
evalHumn :: MonkeyName -> MonkeyMap -> Int
evalHumn mName mMap = 
    if hMap ! mr1
        then determine (mMap ! mr1) (eval mr2 mMap)
        else determine (mMap ! mr2) (eval mr1 mMap)
    where
        mr1 = ((\(MonkeyOp mr1 _ _) -> mr1) (mMap ! mName))
        mr2 = ((\(MonkeyOp _ _ mr2) -> mr2) (mMap ! mName))
        hMap = mkHumnMap mMap
        determine (MonkeyOp m1 op m2) val = 
            -- human is one of two monkeys
            if m1 == "humn" || m2 == "humn"
                then if m1 == "humn"
                    -- human in 'monkey' on the left
                    then eval_op_inv op val (eval m2 mMap)
                    -- human in 'monkey' on the right
                    else if (op == Div) || (op == Min)
                        then eval_op op (eval m1 mMap) val
                        else eval_op_inv op val (eval m1 mMap)
                else if hMap ! m1 
                    -- human is somewhere on the left subtree
                    then 
                        (determine (mMap ! m1) (eval_op_inv op val (eval m2 mMap)))
                    -- human is somewhere on the right subtree
                    else if (op == Div) || (op == Min)
                        then (determine (mMap ! m2) (eval_op op (eval m1 mMap) val))
                        else (determine (mMap ! m2) (eval_op_inv op val (eval m1 mMap)))

-- invert operation 
eval_op_inv :: Op -> Int -> Int -> Int
eval_op_inv Add m n = m - n
eval_op_inv Min m n = m + n 
eval_op_inv Mul m n = m `div` n
eval_op_inv Div m n = m * n  

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = eval "root" mMap
    where mMap = head $ parseAll monkeyMap s
-- answer 1 = 232974643455000

-- solution for part 2
answer2 :: String -> Int
answer2 s = evalHumn "root" mMap
    where mMap = head $ parseAll monkeyMap s
-- answer 2 = 3740214169961 
       
-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input