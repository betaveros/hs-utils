#!/usr/bin/env runhaskell
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Process
import System.IO

looper :: String -> String -> String
looper _ [] = []
looper (x:xs) (r:rs)
    | isSpace r = r : looper (x:xs) rs
    | otherwise = x : looper xs rs
looper [] _ = error "Empty string to loop"

readShellProcess :: String -> IO String
readShellProcess s = do
    (_, Just hout, _, _) <-
        createProcess (shell s) { std_out = CreatePipe }
    hGetContents hout

printUsage :: IO ()
printUsage = do
    putStrLn "usage: looper.hs text"
    putStrLn "       looper.hs -t text"
    putStrLn "       looper.hs -f file"
    putStrLn "       looper.hs -c command"

main :: IO ()
main = do
    args <- getArgs
    if null args
    then printUsage
    else do
        loop <- case head args of
            "-f" -> readFile $ args !! 1
            "-c" -> readShellProcess (args !! 1)
            "-t" -> return $ args !! 1
            _    -> return $ concat args
        interact $ looper $ cycle $ filter (not . isSpace) loop

-- figlet -f block Hello world! | looper.hs hello
