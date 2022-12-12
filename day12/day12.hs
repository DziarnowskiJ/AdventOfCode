module Day12 where 

import System.IO
import Geometry
import Search
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- a maze has a set of open locations, a start point and a goal
type Hill = (MapLocation, Location, Location)

type Location = (Point, Char)

type MapLocation = Map Point Char

-- interpret a multiline string as a hill
readHill :: String -> Hill
readHill s =
    (Map.fromList (readGrid s),
    head [(p, c) | (p, c) <- readGrid s, c == 'S'],
    head [(p, c) | (p, c) <- readGrid s, c == 'E'])

-- finds a shortest path between two points and returns its length
-- if the path is not found, returns -1
pathLength :: Hill -> Int
pathLength (ls, start, finish)  
    | any (\x -> x == True) $ map (\x -> testNeighbour ls finish x) $ Set.toList $ last ss = length ss
    | otherwise = -1
    where ss = solve (ls, start, finish)

-- points reached in each step from the start to the goal
solve :: Hill -> [Set Location]
solve (open, start, finish) = 
    takeWhile (\ s -> not (Set.member finish s)) $ 
    bfs (moves open) start

-- possible moves in the set from the point
moves :: MapLocation -> Graph Location
moves open (p, c) = 
    Set.fromList $
    [ (point, open Map.! point) | 
        -- reachable points
        point <- Set.toList $ Set.intersection (Set.fromList $ Map.keys open) (neighbours p),
        -- check 'hight' accessibility
        customOrd c + 1 >= customOrd (open Map.! point)]

-- neighbours of a point in each compass direction
neighbours :: Point -> Set Point
neighbours p = Set.fromList [ plusPoint p (oneStep d) | d <- [North, East, South, West]]

-- tests whether (p2, c2) is a accessible from (p1, c1)
testNeighbour :: MapLocation -> Location -> Location -> Bool
testNeighbour ls (p1, c1) (p2, c2) = 
    Set.member (p2, c2) $ 
    Set.fromList $ 
    [(p, ls Map.! p) | p <- Set.toList $ neighbours p1]

-- customize Data.Char 'ord' function
-- all chars return the same value as normally,
-- except for 'S' and 'E', those return values of 'a' and 'z' 
customOrd :: Char -> Int
customOrd x 
    | x == 'S' = ord 'a'
    | x == 'E' = ord 'z'
    | otherwise = ord x

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

------------------------------
-- LESS EFFICIENT SOLUTION 
-- (takes ~20 min to compute)
------------------------------

-- REASONING
-- when the input file is inspected, it is clear that
-- there are a lot of occurances of 'a', however
-- there are much less 'a' that are direct neighbours of 'b'
-- 
-- closer inspection of the input leads to discovery that 
-- 'b's are only in the second column and first column is
-- comletly filled with 'a's. That means that possible path
-- starts with one of those 'a's from the first column.
-- 
-- So the shortest possible path from any 'a' to 'E'
-- will be just one longer than shortest path from any 'b' to 'E'
-- this decreases the number of start points that need to be inspected
-- from 2063 to only 41
--
-- Unfortunatelly, it is still a lot to process and this solution 
-- is only partially optimal

-- interpret a multiline string as a hill
-- in this case hill can start at any possible 'b'
readAllHills :: String -> [Hill]
readAllHills s =
    [(Map.fromList pcs,
    hillStart,
    head [(p, c) | (p, c) <- readGrid s, c == 'E']) | 
        hillStart <- [(p, c) | (p, c) <- readGrid s, c == 'b']]
    where
        pcs = [(p, c) | (p, c) <- readGrid s]

------------------------------
-- MORE EFFICIENT SOLUTION 
------------------------------

-- finds a shortest path between two points and returns its length
-- if the path is not found, returns -1
-- only difference with 'pathLength' is that it uses 'solve2' instead of 'solve'
-- this creates a possible move from 'S' to any 'a' on the map
pathLength2 :: Hill -> Int
pathLength2 (ls, start, finish)  
    | any (\x -> x == True) $ map (\x -> testNeighbour ls finish x) $ Set.toList $ last ss = length ss
    | otherwise = -1
    where ss = solve2 (ls, start, finish)

-- points reached in each step from the start to the goal
-- only difference with 'solve' is that it uses 'moves2' instead of 'moves'
-- this creates a possible move from 'S' to any 'a' on the map
solve2 :: Hill -> [Set Location]
solve2 (open, start, finish) =
    takeWhile (\ s -> not (Set.member finish s)) $
    bfs (moves2 open) start

-- possible moves in the set from the point
-- only difference with 'moves' is that it uses 'neighbours2' instead of 'neighbours'
-- this creates a possible move from 'S' to any 'a' on the map
moves2 :: MapLocation -> Graph Location
moves2 open (p, c) = 
    Set.fromList $
    [ (point, open Map.! point) | 
        -- reachable points
        point <- Set.toList $ Set.intersection (Set.fromList $ Map.keys open) (neighbours2 (p, c) open),
        -- check 'hight' accessibility
        customOrd c + 1 >= customOrd (open Map.! point)]

-- neighbours of a point in each compass direction
-- the difference here is that 'S' has one-way connection with any 'a'
neighbours2 :: (Point, Char) -> MapLocation -> Set Point
neighbours2 (p, c) ls
    | c /= 'S' = Set.fromList [ plusPoint p (oneStep d) | d <- [North, East, South, West]]
    | otherwise = 
        Set.union 
            (Set.fromList [p | (p, c) <- Map.toList ls, (ls Map.! p == 'a' || ls Map.! p == 'S')])
            (neighbours2 (p, 'a') ls)

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
-- save version of the answer, returns -1 if the path is not found
answer1 s = pathLength $ readHill s
-- answer 1 = 534
    -- unsave version (returns wrong length is path is not found)
    -- answer1 s = length $ solve $ readHill s

-- solution for part 2
answer2 :: String -> Int
-- save version of the answer, returns -1 if the path is not found
answer2 s = (\x -> if x - 1 == answer1 s then x else x - 1) . pathLength2 $ readHill s
-- answer 2 = 525
    -- unsave version (returns wrong length is path is not found)
    -- answer2 s = (\x -> if x - 1 == answer1 s then x else x - 1) . length $ solve2 $ readHill s
    -- alternative answer2
    -- answer2 s = minimum $ map (\x -> x + 1) $ map pathLength (readAllHills s)

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input