module Day17 where 

import System.IO
import Geometry
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------
 {-
 Shapes of rocks:
 (they fall in this pattern)

 ####    #      #   #   ##
        ###     #   #   ##
         #    ###   #
                    #
       
 Cave:
 (forever extending upward)

 |.......|
 |.......|
 |.......|
 +-------+          
 
 Jet streams move rocks:
 - < - to the left
 - > - to the right

 -}

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define type Rock (as a list of Points)
type Rock = [Point]

-- define type Cave (as a list of points)
type Cave = Set Point

-- define initial cave 
-- (no need to implement walls as other functions assume they are at x = -1 and x = 7)
initCave :: Cave
initCave = Set.fromList $ take 7 $ iterate (\(Point x y) -> Point (x+1) y) (Point 0 (-1))

-- ####
rock1 :: Int -> Rock
rock1 = (\n -> [Point 2 n, Point 3 n, Point 4 n, Point 5 n])

--  # 
-- ###
--  # 
rock2 :: Int -> Rock
rock2 = (\n -> [Point 3 n, Point 2 (n+1), Point 3 (n+1), Point 4 (n+1), Point 3 (n+2)])

--   #
--   #
-- ###
rock3 :: Int -> Rock
rock3 = (\n ->  [Point 2 n, Point 3 n, Point 4 n, Point 4 (n+1), Point 4 (n+2)])

-- #
-- #
-- #
-- #
rock4 :: Int -> Rock
rock4 = (\n ->  [Point 2 n, Point 2 (n+1), Point 2 (n+2), Point 2 (n+3)])

-- ##
-- ##
rock5 :: Int -> Rock
rock5 = (\n ->  [Point 2 n, Point 3 n, Point 2 (n+1), Point 3 (n+1)])

-- return rock 
-- takes two arguments: 
--      - rock to be returned
--      - lowest point of this rock
rocks :: Int -> (Int -> Rock)
rocks n = head $ drop (((n-1) `mod` 5)) [rock1, rock2, rock3, rock4, rock5]

-- move rock horizontally by n
pushRock :: Rock -> Int -> Rock
pushRock r n = [(Point (x+n) y) | (Point x y) <- r]

-- move rock one step down
fallRock :: Rock -> Rock
fallRock r = [(Point x (y-1)) | (Point x y) <- r]

-- returns position of new rock and wheter it can be placed there
canPushRock :: Cave -> Rock -> Int -> (Rock, Bool)
canPushRock cave rock n = 
    (newRock, 
    all (== True) [Set.notMember newPoint cave && x > -1 && x < 7
                | newPoint@(Point x y) <- newRock])
    where newRock = pushRock rock n

-- returns position of new rock and wheter it can be placed there
canFallRock :: Cave -> Rock -> (Rock, Bool)
canFallRock cave rock = 
    (newRock,
    all (== True) [Set.notMember newPoint cave | newPoint <- newRock])
    where newRock = fallRock rock

-- return highest point of the static rocks
highestPoint :: Cave -> Int
highestPoint c = maximum [ y | (Point x y) <- Set.toList c]

-- return final position of the falling rock and the movements that will follow next
restingRockPos :: Cave -> Rock -> String -> (Rock, String)
restingRockPos cave rock movement = 
    if snd movedRock 
        -- rock CAN be moved
        then if snd $ canFallRock cave $ fst movedRock
            -- rock CAN fall
            then 
                restingRockPos 
                    cave (fst $ canFallRock cave $ fst movedRock) (tail movement ++ [head movement])
            -- rock CANNOT fall
            else (fst movedRock, (tail movement ++ [head movement]))
        -- rock CANNOT be moved
        else if snd $ canFallRock cave rock
            -- rock CAN fall
            then 
                restingRockPos
                    cave (fst $ canFallRock cave rock) (tail movement ++ [head movement])
            -- rock CANNOT fall
            else (rock, (tail movement ++ [head movement]))
    where 
        move = if head movement == '<' then -1 else 1
        movedRock = canPushRock cave rock move

-- create a falling rock and add its final position to the cave
-- return the new cave and movements (jets) that will go next
placeRock :: Cave -> Int -> String -> (Cave, String)
placeRock cave rockNo movement = 
    (Set.union cave (Set.fromList $ fst finPos), snd finPos)
    where finPos = restingRockPos cave (rocks rockNo (4 + highestPoint cave)) movement

-- place X number of rocks and return final cave with movements that would be next
placeRocks :: Cave -> Int -> String -> (Cave, String)
placeRocks cave 0 moves = (cave, moves)
placeRocks cave movesCount moves = placeRock (fst ret) (movesCount) (snd ret)
    where ret = placeRocks cave (movesCount -1) moves


-- convert Cave to its graphical (string) form
printCave :: Cave -> String
printCave cave = printGrid $ map (\x -> (x,'#')) (Set.toList cave)

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- Because of the fact that both input (jet directions) and rocks
-- are constantly repeating, at some point there must be a 
-- repeating cycle such that rocks will create same confuguration
-- in case of test input this cycle has length 35 
-- (meaning that after some time, the top of the cave will look exactly the same
-- as 35 rocks ago)
-- In the case of my input it is 1725 (this comes from my investigation)
cycleSize :: Int
cycleSize = 1725

cycleSizeTest :: Int
cycleSizeTest = 35

-- return height of the rock pile after many drops
caveHeight :: Int -> String -> Int -> Int
caveHeight cycSize movement dropNo = (a * b + c) + 1
    where 
        a = dropNo `div` cycSize
        b = (highestPoint $ fst $ placeRocks initCave (cycSize + dropNo `mod` cycSize) movement) - c
        c = highestPoint $ fst $ placeRocks initCave (dropNo `mod` cycSize) movement

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = (+1) . highestPoint $ fst $ placeRocks initCave 2022 s
-- answer 1 = 3157

-- solution for part 2
answer2 :: Int -> String -> Int
answer2 x s = caveHeight x s 1000000000000
-- answer 2 = 1581449275319

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    if path == "day17/input.txt" 
        then putStrLn $ show $ answer2 cycleSize input
        else putStrLn $ show $ answer2 cycleSizeTest input
