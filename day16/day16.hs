module Day16 where 

import System.IO
import Parser
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Search

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- parse Valve
valve :: Parser Valve
valve = 
    (\x y z -> Valve {
        name = x,
        flow = y,
        connects = z
    })
    <$ string "Valve " 
    <*> valveId 
    <* string " has flow rate="
    <*> int
    <* (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
    <*> sepBy1 valveId (string ", ")

valveId :: Parser String
valveId = (\x y -> [x,y]) <$> satisfy (isUpper) <*> satisfy (isUpper)

manyValves :: Parser Valves
manyValves = (\x -> Map.fromList [(name y, y) | y <- x ]) <$> sepBy1 valve (char '\n')

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define Valve (ID, FlowRate, [Leads to IDs])
data Valve = Valve {
    name :: String,
    flow :: Int,
    connects :: [String]
    } 
    deriving (Show, Eq, Ord)

-- define Valves as map of strings to valves
type Valves = Map String Valve

-- points reached in each step from the start to the goal
distanceSet :: (Valves, Valve) -> [Set Valve]
distanceSet (allValves, start) =
    bfs (moves allValves) start

-- possible moves in the set from the point
moves :: Valves -> Graph Valve
moves allValves start = (neighbours allValves start)

-- neighbours of a point in each compass direction
neighbours :: Valves -> Valve -> Set Valve
neighbours allValves start =
    Set.fromList [allValves Map.! v | v <- connects start]

distanceMap :: Valves -> [Set Valve] -> Map String Int
distanceMap allVs vs = 
    Map.filterWithKey (\k _ -> ((flow $ allVs Map.! k) > 0) || (k == "AA")) $ 
    go (vs) 1
    where 
        go [] x = Map.empty
        go (v:vs) x = 
            Map.union 
            (Map.fromList [ (name n, x) | n <- Set.toList v])
            (go vs (x + 1))

type DistDict = Map String (Map String Int)

distDict :: Valves -> DistDict
distDict vs = 
    Map.fromList $
    [(k, distanceMap vs (distanceSet (vs, v))) | (k, v) <- Map.toList vs, (flow v > 0 || name v == "AA")]

infixl 6 !!:
(!!:) :: Map String (Map String Int) -> (String, String) -> Int
(bigMap) !!: (key1, key2) = (bigMap Map.! key1) Map.! key2

maxFlow :: Valves -> Int -> Int
maxFlow valves time = go "AA" Set.empty time 
    where 
        distances = distDict valves    
        go gCurrent opened minLeft
            | minLeft <= 0 = 0
            | Set.notMember gCurrent opened =
                let val = minLeft * (flow $ valves Map.! gCurrent)
                    curOpened = Set.insert gCurrent opened
                in maximum $
                [val + (go
                        x
                        curOpened
                        (minLeft - (distances !!: (x, gCurrent))))    
                | x <- Map.keys distances]
            | otherwise = 0

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

splitValves :: [String] -> [([String], [String])]
splitValves ss = 
    take ((length splits) `div` 2) $
    sort $
    splits
    where 
        sSet = Set.fromList ss
        ps = Set.powerSet sSet
        splits = [ (Set.toList (Set.difference sSet s), Set.toList s) | s <- Set.toList ps, length s > 0, length s < length ss]

maxFlow2 :: Valves -> Int -> [String] -> Int
maxFlow2 valves time valvesToCheck = go "AA" Set.empty time 
    where 
        distances = distDict valves    
        go gCurrent opened minLeft
            | minLeft <= 0 = 0
            | Set.notMember gCurrent opened =
                let val = minLeft * (flow $ valves Map.! gCurrent)
                    curOpened = Set.insert gCurrent opened
                in maximum $
                [val + (go
                        x
                        curOpened
                        (minLeft - (distances !!: (x, gCurrent))))    
                | x <- valvesToCheck ]
            | otherwise = 0

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = maxFlow (head $ parseAll manyValves s) 30
-- answer 1 = 

-- solution for part 2
answer2 :: String -> Int
answer2 s = 
    maximum [maxFlow2 inp 26 xs + maxFlow2 inp 26 ys |
    (xs, ys) <- splitValves $ Map.keys $ distDict inp]
        where inp = (head $ parseAll manyValves s)
-- answer 2 = 

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input