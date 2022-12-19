module Day where 

import System.IO
import Parser
import Data.List 
import Data.Set (Set)
import qualified Data.Set as Set
import Search

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

cube :: Parser Cube
cube = (\x y z -> (x, y, z)) <$> int <* char ',' <*> int <* char ',' <*> int

manyCubes :: Parser [Cube]
manyCubes = sepBy1 cube (string "\n")

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define Cube type (X-dim, Y-dim, Z-dim)
type Cube = (Int, Int, Int)

-- possible adjacent cubes of cube (0,0,0)
adjFun :: [Cube]
adjFun = [(0,0,1), (0,0,(-1)),
          (0,1,0), (0,(-1),0),
          (1,0,0), ((-1),0,0)]

-- return list of adjacent cubes to the input cube
adjCube :: Cube -> [Cube]
adjCube cube = [addCube cube adj | adj <- adjFun]
    where addCube (x, y, z) (a, b, c) = (x+a, y+b, z+c)

-- return number of sides that are exposed to the air
exposed :: [Cube] -> Int
exposed cubes = go cubes
    where 
        go [] = 0
        go (c:cs) = ((6 -) . length $ [a | a <- adjCube c, a `elem` cubes ]) + (go cs)


--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- define ranges type (min X, max X), (min Y, max Y), (min Z, max Z) 
type Ranges = ((Int, Int),(Int, Int),(Int, Int))

-- return tuple of 3 ranges (min X, max X), (min Y, max Y), (min Z, max Z) 
ranges :: [Cube] -> Ranges
ranges cube = ((minimum xs, maximum xs), (minimum ys, maximum ys), (minimum zs, maximum zs))
    where 
        xs = [x | (x, _, _) <- cube] 
        ys = [y | (_, y, _) <- cube]
        zs = [z | (_, _, z) <- cube]

-- create a list of points that are included in a cuboid
-- of ranges (min X, max X), (min Y, max Y), (min Z, max Z) 
fullCube :: Ranges -> [Cube]
fullCube rang@(xs, ys, zs) = 
    [(x, y, z) | 
        x <- [(fst xs)-1..1+(snd xs)],
        y <- [(fst ys)-1..1+(snd ys)],
        z <- [(fst zs)-1..1+(snd zs)]]

-- remove second shape from the first one
extractShape :: [Cube] -> [Cube] -> [Cube]
extractShape shape1 shape2 = 
    Set.toList $ 
    Set.difference 
    (Set.fromList shape1)
    (Set.fromList shape2)

-- return all unreachable points 
-- (points that are not connected by a full side
-- diagonal connection does not count)
unreachablePoints :: [Cube] -> Set Cube
unreachablePoints shape@(c:cs) = 
    Set.difference 
    (Set.fromList shape)
    (Set.unions (bfs (moves shape) c))

-- possible moves in the set from the point
moves :: [Cube] -> Graph Cube
moves open p = Set.intersection (Set.fromList open) (Set.fromList $ adjCube p)

-- return 'air pockets' in cube
airPockets :: [Cube] -> [Cube]
airPockets shape = Set.toList $ unreachablePoints $ extractShape (fullCube $ ranges shape) shape

-- eliminate all 'air pockets' from the shape
fillShape ::[Cube] -> [Cube] 
fillShape shape = 
    Set.toList $
    Set.union 
    (Set.fromList shape)
    (Set.fromList $ airPockets shape)

-- return exterior surface area of the shape
outsideExposed :: [Cube] -> Int
outsideExposed shape = exposed $ fillShape shape

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = exposed (head $ parseAll manyCubes s)
-- answer 1 = 4300

-- solution for part 2
answer2 :: String -> Int
answer2 s = outsideExposed (head $ parseAll manyCubes s)
-- answer 2 = 2490

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStr "Part 2: "
    putStrLn $ show $ answer2 input