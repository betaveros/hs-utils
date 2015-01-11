#!/usr/bin/env runhaskell
import Data.Char (chr)

red :: String
red = chr 27 : "[0;30;41m"
blue :: String
blue = chr 27 : "[0;30;44m"
nocolor :: String
nocolor = chr 27 : "[0m"

putLine :: String -> IO ()
putLine color = putStrLn $ color ++ replicate 72 '-' ++ nocolor

main :: IO ()
main = do
    putStrLn ""
    putLine red
    putStrLn ""
    putLine blue
    putStrLn ""
