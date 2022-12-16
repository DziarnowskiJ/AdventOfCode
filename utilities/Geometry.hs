module Geometry where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

-- compass points
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving Show

-- the direction immediately to the left
turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft NE = NW
turnLeft E = N
turnLeft SE = NE
turnLeft S = E
turnLeft SW = SE
turnLeft W = S
turnLeft NW = SW

-- the direction immediately to the right
turnRight :: Direction -> Direction
turnRight N = E
turnRight NE = SE
turnRight E = S
turnRight SE = SW
turnRight S = W
turnRight SW = NW
turnRight W = N
turnRight NW = NE

-- x and y coordinates in two-dimensional space
data Point = Point Int Int
    deriving (Eq, Ord, Show)

-- the origin of the two-dimensional space
origin :: Point
origin = Point 0 0

-- add two points
plusPoint :: Point -> Point -> Point
plusPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

-- subtract two points
minusPoint :: Point -> Point -> Point
minusPoint (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

-- multiply both components of a point by a given number
timesPoint :: Int -> Point -> Point
timesPoint n (Point x y) = Point (n*x) (n*y)

-- Manhattan metric of the point
normPoint :: Point -> Int
normPoint (Point x y) = abs x + abs y

-- distance between two points using the Manhattan metric
distance :: Point -> Point -> Int
distance p1 p2 = normPoint (minusPoint p1 p2)

-- the point one unit from the origin in the given direction
oneStep :: Direction -> Point
oneStep N = Point 0 1
oneStep NE = Point 1 1
oneStep E = Point 1 0
oneStep SE = Point 1 (-1)
oneStep S = Point 0 (-1)
oneStep SW = Point (-1) (-1)
oneStep W = Point (-1) 0
oneStep NW = Point 1 (-1)

-- map lines of text onto two-dimensional space, starting at the origin
readGrid :: String -> [(Point, Char)]
readGrid s =
    [(Point x y, c) |
        (y, l) <- zip [0,-1..] (lines s),
        (x, c) <- zip [0..] l]

-- return tuple of Points of top-left corner and bottom righ
-- (bounding corners of 2D grid)
gridDimentions :: [(Point, Char)] -> (Point, Point)
gridDimentions ps = (Point (minimum xs) (maximum ys), Point (maximum xs) (minimum ys))
    where
        xs = [x | ((Point x y), c) <- ps]
        ys = [y | ((Point x y), c) <- ps]

-- creates full grid of chars ' '
emptyGrid :: (Point, Point) -> [(Point, Char)]
emptyGrid ((Point x1 y1), (Point x2 y2)) =
    [(Point x y, ' ') | 
        x <- [xL..xH],
        y <- [yL..yH]]
    where 
        xL = min x1 x2
        xH = max x1 x2
        yL = min y1 y2
        yH = max y1 y2

-- adds (Point, ' ') to all empty spaces in the grid's bounds
fillGrid :: [(Point, Char)] -> [(Point, Char)]
fillGrid ps = Map.toList $ Map.union (Map.fromList ps) (Map.fromList $ emptyGrid $ gridDimentions ps)

-- returns string representation of the grid
printGrid :: [(Point, Char)] -> String
printGrid ps = 
    "TL = " ++ (show (fst $ gridDimentions ps)) ++ "\n" ++ 
    "BR = " ++ (show (snd $ gridDimentions ps)) ++ "\n" ++ 
    unlines  
    (reverse $ 
    transpose $ 
    lines $ 
    prepareToPrint $ 
    sort $ 
    fillGrid $ 
    ps)

-- required to print a grid
prepareToPrint :: [(Point, Char)] -> String
prepareToPrint [] = ""
prepareToPrint ((Point x y, c):[]) = [c] ++ "\n"
prepareToPrint ((Point x1 y1, c1):(Point x2 y2, c2):ps) = 
    if (x1 == x2) 
        then [c1] ++ prepareToPrint ((Point x2 y2, c2):ps)
        else [c1] ++ "\n" ++ prepareToPrint ((Point x2 y2, c2):ps)
