module Day where 

import System.IO
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

convertInput :: String -> [(Char, Int)] 
convertInput ss = [(head s, read (drop 2 s) :: Int) | s <- lines ss]

instr :: (Char, Int) -> [Point]
instr (s, n) = take n $ repeat $ getStepFromChar s

getStepFromChar :: Char -> Point
getStepFromChar c = case c of 
    'U' -> oneStep N
    'R' -> oneStep E
    'D' -> oneStep S
    'L' -> oneStep W

fullInstr :: String -> [Point]
fullInstr s = foldl (\x y -> x ++ instr y) [] (convertInput s)

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

--POINT STUFF

-- Enumerated type Direction with eight values, 
-- the cardinal compass points
data Direction
    = N | E | S | W | NE | NW | SE | SW
    deriving (Show)

-- Define Point
type Point = (Int, Int)

-- Define origin
origin :: Point
origin = (0, 0)

-- Add two points
plusPoint :: Point -> Point -> Point
plusPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- Subtract two points
minusPoint :: Point -> Point -> Point
minusPoint (x1, y1) (x2, y2) = (x1-x2, y1-y2)

-- Map each direction to the point 
-- one unit from the origin in that direction
oneStep :: Direction -> Point
oneStep d = case d of
    N  -> (0,1)
    NE -> (1,1)
    E  -> (1,0)
    SE -> (1,-1)
    S  -> (0,-1)
    SW -> (-1,-1)
    W  -> (-1,0)
    NW -> (-1,1)

--------------------------------------------------------------------------------

-- LINE STUFF

-- define line
type Line = (Point, Point)

-- define starting line
initLine :: Line
initLine = (origin, origin)

-- return point to which tail must be adjusted
adjustTailStep :: Line -> Point
adjustTailStep (h, t) = 
    case minusPoint h t of
    -- allowed positions (tail does not change)
    (0,0)   -> origin       -- (0,0)
    (0,1)   -> origin       -- (0,0)
    (1,1)   -> origin       -- (0,0)
    (1,0)   -> origin       -- (0,0)
    (1,-1)  -> origin       -- (0,0)
    (0,-1)  -> origin       -- (0,0)
    (-1,-1) -> origin       -- (0,0)
    (-1,0)  -> origin       -- (0,0)
    (-1,1)  -> origin       -- (0,0)
    -- stretching line in one plane (horizontal or vertical)
    (0,2)   ->  oneStep N   -- (0,1)
    (0,-2)  ->  oneStep S   -- (0,-1)
    (2,0)   ->  oneStep E   -- (1,0)
    (-2,0)  ->  oneStep W   -- (-1,0)
    -- diagonal stretch ()
    (1,2)   ->  oneStep NE  -- (1,1)
    (2,1)   ->  oneStep NE  -- (1,1)
    (1,-2)  ->  oneStep SE  -- (1,-1)
    (2,-1)  ->  oneStep SE  -- (1,-1)
    (-1,-2) ->  oneStep SW  -- (-1,-1)
    (-2,-1) ->  oneStep SW  -- (-1,-1)
    (-2,1)  ->  oneStep NW  -- (-1,1)
    (-1,2)  ->  oneStep NW  -- (-1,1)
    -- double diagonal (for chain)
    (2,2)   ->  oneStep NE  -- (1,1)
    (2,-2)  ->  oneStep SE  -- (1,-1)
    (-2,-2) ->  oneStep SW  -- (-1,-1)
    (-2,2)  ->  oneStep NW  -- (-1,1)

-- move head of line by one step, and adjust the tail
moveHead :: Line -> Point -> Line
moveHead (h, t) step = adjustTail (plusPoint h $ step, t)

-- adjusts a tail so that it is one step behind head
adjustTail :: Line -> Line
adjustTail (h, t) = (h, plusPoint t $ adjustTailStep (h, t))

-- return point at which the head is
extractHead :: Line -> Point
extractHead (h, t) = h

-- return point at which the tail is
extractTail :: Line -> Point
extractTail (h, t) = t

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- CHAIN STUFF

-- define chain
data Chain = Chain (Line, Chain) | LastChain (Line)
    deriving (Show)

-- recursivly create a chain of N Lines 
-- (head and tail of each line is at (0,0))
initChain :: Int -> Chain
initChain 1 = LastChain (initLine)
initChain n = Chain (initLine, initChain (n-1) )

-- set head at new position 
-- NOTE: new head should be in place accessible by old head in one step
--      (however it is not checked for)
setHead :: Line -> Point -> Line
setHead (h, t) p = adjustTail (p, t)

-- move chain 
-- moves the head of the first line and recursivly adjusts the rest of the chain
-- st. tail of the line is the position at which head of the next line will be placed
updateChain :: Chain -> Point -> Chain
updateChain (LastChain line) point = LastChain (setHead line point)
updateChain (Chain (line, chain)) point = 
    Chain ((setHead line point), updateChain chain $ extractTail (setHead line point))

-- return position of head after the line is moved 
-- (simulates moveHead, and returns head)
newHead :: Line -> Point -> Point
newHead line point = extractHead $ moveHead line point

-- move the head of the first Line in a chain
-- and adjust following Lines accordingly
moveChain :: Chain -> Point -> Chain
moveChain (Chain (line, chain)) point = 
    updateChain (Chain (line, chain)) $ newHead line point

-- get last knot in a chain
extractLast :: Chain -> Point
extractLast (LastChain line) = extractTail line
extractLast (Chain (line, chain)) = extractLast chain

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = 
    Set.size $
    Set.fromList $
    map extractTail (scanl (\x y -> moveHead x y) initLine (fullInstr s))
-- answer 1 = 5858

-- solution for part 2
answer2 :: String -> Int
answer2 s = 
    Set.size $
    Set.fromList $
    map extractLast (scanl (\x y -> moveChain x y) (initChain 9) (fullInstr s))
-- answer 2 = 2602

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input