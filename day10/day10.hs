module Day10 where 

import System.IO
import Data.Char

--------------------------------------------------------------------------------
-- INPUT PROCESSING
--------------------------------------------------------------------------------

-- define command
data Command = Noop | Add Int
    deriving Show

-- convert input into List of Commands
-- "noop" -> [Noop] and "addx X" -> [Noop, Add X]
convertInput :: String -> [Command]
convertInput ss = foldl (\x y -> x ++ if head y == 'n' 
                                    then [Noop] 
                                    else [Noop] ++ [Add (read (drop 5 y) :: Int)] ) 
                                [] (lines ss)

--------------------------------------------------------------------------------
-- PART 1
--------------------------------------------------------------------------------

-- define process as tuple of two Int
-- (Cycle, X register)
type Process = (Int, Int)

-- define initial process
initProcess :: Process
initProcess = (1, 1)

-- perform command on process
action :: Process -> Command -> Process
action (cycle, x) Noop = (cycle + 1, x)
action (cycle, x) (Add y) = (cycle + 1, x + y)

-- perform list of commands on process
-- and return list of all intermediate processes
execute :: Process -> [Command] -> [Process]
execute process commands = init $ scanl action process commands

-- return sum of signals that are on process no 20, 60, 100 ...
signals :: [Process] -> Int
signals ps = sum [c * x | (c,x) <- ps, (c - 20) `mod` 40 == 0 ] 

--------------------------------------------------------------------------------
-- PART 2
--------------------------------------------------------------------------------

-- converts list of processes (cycle, register) into string format
-- st. if cycle is range of (register -1, register +1) it puts 
drawLine :: Char -> Char -> [Process] -> String
drawLine t f ps = [ if (x - 1 <= ((c - 1) `mod` 40) && 
                        ((c - 1) `mod` 40) <= x + 1) 
                        then t else f | (c,x) <- ps]

-- puts newLine characters ('\n') in provided string spaced out every 40 characters
splitLine :: String -> String
splitLine [] = []
splitLine s = take 40 s ++ "\n" ++ (splitLine $ drop 40 s)

--------------------------------------------------------------------------------
-- SHOW ANSWERS
--------------------------------------------------------------------------------

-- -- solution for part 1
answer1 :: String -> Int
answer1 s = signals $ execute initProcess $ convertInput s
-- answer 1 = 15220

-- solution for part 2
answer2 :: String -> String
answer2 s = splitLine $ drawLine '#' '.' $ execute initProcess $ convertInput s

answer2_clean :: String -> String
answer2_clean s = splitLine $ drawLine '#' ' ' $ execute initProcess $ convertInput s
-- answer 2 = RFZEKBFA
{-
###..####.####.####.#..#.###..####..##..
#..#.#.......#.#....#.#..#..#.#....#..#.
#..#.###....#..###..##...###..###..#..#.
###..#.....#...#....#.#..#..#.#....####.
#.#..#....#....#....#.#..#..#.#....#..#.
#..#.#....####.####.#..#.###..#....#..#.

###  #### #### #### #  # ###  ####  ##
#  # #       # #    # #  #  # #    #  #
#  # ###    #  ###  ##   ###  ###  #  #
###  #     #   #    # #  #  # #    ####
# #  #    #    #    # #  #  # #    #  #
#  # #    #### #### #  # ###  #    #  #
-}

-- show solutions
compute :: FilePath -> IO ()
compute path = do 
    input <- readFile path
    putStr "Part 1: "
    putStrLn $ show $ answer1 input
    putStrLn "Part 2: "
    putStrLn $ answer2 input
    putStrLn "Part 2 (cleaner colution):"
    putStrLn $ answer2_clean input

