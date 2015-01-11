#!/usr/bin/env runhaskell
import Control.Monad (forM_)
import Data.Char (ord, chr)

shiftAgainst :: Int -> Char -> Char -> Char
shiftAgainst n b c = chr $ ob + (oc - ob + n) `mod` 26
    where ob = ord b; oc = ord c

shift :: Int -> Char -> Char
shift n c
    | 'a' <= c && c <= 'z' = shiftAgainst n 'a' c
    | 'A' <= c && c <= 'Z' = shiftAgainst n 'A' c
    | otherwise = c

main :: IO ()
main = do
    s <- getContents
    forM_ [0..25] $ \n -> do
        putStr (show n ++ ": ")
        putStrLn $ map (shift n) s
