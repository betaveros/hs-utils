#!/usr/bin/env runhaskell
main :: IO ()
main = interact $ unlines . reverse . lines
