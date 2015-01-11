#!/usr/bin/env runhaskell
import System.Environment (getArgs)

replicateEach :: Int -> [a] -> [a]
replicateEach k = concatMap (replicate k)

getDims :: [String] -> (Int, Int)
getDims []       = (2, 2)
getDims [s]      = (k, k) where k = read s
getDims [s1, s2] = (k, m) where k = read s1; m = read s2
getDims _        = error "square.hs: too many arguments"

main :: IO ()
main = do
    args <- getArgs
    let (k, m) = getDims args
        in interact $ unlines . replicateEach m . map (replicateEach k) . lines
