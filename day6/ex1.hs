module Ex1 where

import System.IO

diffFour :: String -> Int
diffFour (x:y:z:w:rs) 
    | x /= y && x /= z && x /= w &&
        y /= z && y /= w &&
        z /= w = 4
    | otherwise = 1 + diffFour (y:z:w:rs)

compute :: FilePath -> IO Int
compute path = do {
    content <- readFile path;
    return $ diffFour content
}