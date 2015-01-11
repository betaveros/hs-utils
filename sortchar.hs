#!/usr/bin/env runhaskell
import System.Environment
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther _ [] = []
mapEveryOther _ [x] = [x]
mapEveryOther f (x:y:xs) = x : f y : xs

sortInQuotes :: String -> String
sortInQuotes = intercalate "\"" . mapEveryOther sort . splitOn "\""

main :: IO ()
main = do
    args <- getArgs
    let f = if "--quote" `elem` args then sortInQuotes else sort
    interact $ unlines . map f . lines
