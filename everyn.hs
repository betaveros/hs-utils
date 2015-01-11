#!/usr/bin/env runhaskell
import System.Environment

readArgs :: [String] -> (Int, Int)
readArgs [] = (2, 0)
readArgs [x] = (read x, 0)
readArgs [x,y] = (read x, read y)
readArgs _ = error "too many args"

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (x:xs) = x : everyNth n (drop (n-1) xs)

main :: IO ()
main = do
    args <- getArgs
    let (n, s) = readArgs args
    interact $ unlines . everyNth n . drop s . lines
