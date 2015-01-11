#!/usr/bin/env runhaskell
import Math.NumberTheory.Primes.Factorisation
import System.Environment

printFactorize :: Integer -> IO ()
printFactorize n = do
    putStr $ show n
    putStr ": "
    print $ factorise n

main :: IO ()
main = do
    args <- getArgs
    if null args
        then getContents >>= mapM_ (printFactorize . read) . lines
        else mapM_ (printFactorize . read) args
