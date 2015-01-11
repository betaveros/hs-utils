#!/usr/bin/env runhaskell
import System.Random
import System.Environment
import Control.Applicative
import Control.Monad

randomLine :: String -> IO String
randomLine s = let ls = lines s in (ls !!) <$> randomRIO (0, length ls - 1)

main :: IO ()
main = do
    args <- getArgs
    txts <- sequence $ case args of
        [] -> [getContents]
        xs -> map readFile xs
    mapM_ (putStrLn <=< randomLine) txts
