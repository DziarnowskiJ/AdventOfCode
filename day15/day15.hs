module Day15 where 

import System.IO
import Parser
import Geometry
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- define sensor (Sensor location, Beacon location, Distance to closest beacon)
type Sensor = (Point, Point, Int)

-- point paarser 
-- (since in Geometry module positive y corresponds to North, 
-- and in input it means down, y value is multiplied by (-1))
point :: Parser Point
point = (\x y -> Point x (y * (-1))) <$ string "x=" <*> int <* string ", y=" <*> int

-- parse sensor
sensor :: Parser Sensor
sensor = (\p1 p2 -> (p1, p2, distance p1 p2)) <$ string "Sensor at " <*> point <* string ": closest beacon is at " <*> point

-- parse many sensors
manySensors :: Parser [Sensor]
manySensors = sepBy1 sensor (string "\n")

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define Range (low, high)
type Range = (Int, Int)

-- return Range which sensor is covering at height n (Y-dimention)
coverRangeY :: Int -> Sensor -> [Range]
coverRangeY n sens@((Point sx sy), b, d) 
    | n > sy && n <= sy + d = [((sx - sy - d + n), (sx + sy + d - n))]
    | n < sy && n >= sy - d = [((sx + sy - d - n), (sx - sy + d + n))]
    | n == sy               = [((sx - d),          (sx + d))]
    | otherwise             = [] 

-- return Range which all sensors are covering at height n (Y-dimention)
coverLineY :: Int -> [Sensor] -> [Range]
coverLineY n ss = concat $ map (coverRangeY n) ss

-- reduce list of Ranges 
-- st. if any ranges are covering the same area they are added together
-- eg. [(1, 3), (2, 7)] --> [(1,7)]
reduceRange :: [Range] -> [Range]
reduceRange rs = merge $ sort rs
    where 
        merge :: [Range] -> [Range]
        merge [] = []
        merge (r:[]) = [r]
        merge (r1@(l1, h1):r2@(l2, h2):rs) 
            | h1 >= l2 = merge ((l1, max h1 h2) : rs)
            | h1 < l2 = [(l1, h1)] ++ merge (r2 : rs)

-- count number of spaces in list of Ranges
countCover :: [Range] -> Int
countCover [] = 0
countCover (r@(l, h):[]) = length [l..h]
countCover (r@(l, h):rs) = length [l..h] + countCover rs

-- count number of beacons on Y line
countBeaconY :: Int -> [Sensor] -> Int
countBeaconY n ss = 
    Set.size $ 
    Set.fromList $
    [ beac | (_, beac@(Point x y), _) <- ss, y == n ]
--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- find location of missing beacon
coverSquare :: Int -> [Sensor] -> (Int, Int)
coverSquare sqMax ss = 
    (\(r, y) -> ((snd $ head r) + 1, (-1) * y)) $ 
    head $
    dropWhile ((== 1) . length . fst) $
    [((reduceRange $ coverLineY y ss), 
    y) 
        | y <- [0,(-1)..sqMax]]

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int -> Int
answer1 s n = (countCover $ reduceRange $ coverLineY n $ sensors) - countBeaconY n sensors
    where sensors = head $ parseAll manySensors s
-- answer 1 = 4907780

-- solution for part 2
answer2 :: String -> Int -> Int
answer2 s n = (\(x, y) -> 4000000 * x + y) $ coverSquare n (head $ parseAll manySensors s)
-- answer 2 = 13639962836448  --> (3409990,2836448)

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Which line you want to inspect (for part 1): "
    notBeac <- getLine
    putStr "What is the max x dimention (for part 2): "
    sqSize <- getLine
    putStr "Part 1: "
    putStrLn $ show $ answer1 input (read notBeac :: Int)
    putStr "Part 2: "
    putStrLn $ show $ answer2 input (read sqSize :: Int)