#!/usr/bin/env runhaskell
import Data.Ord (comparing)
import Data.Monoid ((<>))
import Data.List (sortBy)

main :: IO ()
main = interact $ unlines . sortBy (comparing length <> compare) . lines
