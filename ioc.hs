#!/usr/bin/env runhaskell
{-# LANGUAGE TupleSections #-}
import Control.Monad
import Text.Printf
import Data.List.Split
import Data.List
import Control.Applicative
import qualified Data.Map as Map

ioc :: (Ord a) => [a] -> Double
ioc = liftA2 (/) ((^2) . sum) (sum . map (^2)) . Map.elems . Map.fromListWith (+) . map (,1::Double)

iocOf :: (Ord a) => Int -> [a] -> Double
iocOf n = liftA2 (/) sum genericLength . map ioc . transpose . chunksOf n

main :: IO ()
main = do
    s <- getContents
    forM_ [1..1000] $ \i -> printf "%d: %f\n" i $ iocOf i s
