#!/usr/bin/env runhaskell

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (groupBy)
import System.Environment

isNumber :: String -> Bool
isNumber (c:_) = isDigit c
isNumber [] = False

isFloatChar :: Char -> Bool
isFloatChar c = isDigit c || c == '.'
isFloat :: String -> Bool
isFloat [] = False
isFloat s@(c:_) = isFloatChar c && length (filter (== '.') s) <= 1 && any isDigit s

sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter isNumber . groupBy ((==) `on` isDigit)

sumFloats :: String -> Float
sumFloats = sum . map read . filter isFloat . groupBy ((==) `on` isFloatChar)

main :: IO ()
main = do
    args <- getArgs
    getContents >>= if "-f" `elem` args
        then print . sumFloats
        else print . sumNumbers
