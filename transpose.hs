#!/usr/bin/env runhaskell

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)

data Options = Options
    { fillChar  :: Char
    , trimLines :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { fillChar  = ' '
    , trimLines = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['f'] ["fill"]
        (ReqArg (\ c opts -> opts { fillChar = head c })
                "char")
        "fill with char"
    , Option ['t'] ["trim"]
        (NoArg (\ opts -> opts { trimLines = True }))
        "trim padded ends of resulting lines"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: transpose.hs [OPTION..]"

-- Haskell's transpose skips short lines; we want to pad them with spaces.
fillTranspose :: a -> [[a]] -> [[a]]
fillTranspose fc xss
    | all null xss = []
    | otherwise    = map (fromMaybe fc . listToMaybe) xss :
        fillTranspose fc (map (drop 1) xss)

trimIf :: (Eq a) => Bool -> a -> [a] -> [a]
trimIf False _  = id
trimIf True  fc = reverse . dropWhile (== fc) . reverse

main :: IO ()
main = do
    args <- getArgs
    (opts, _) <- compilerOpts args
    let
        fc = fillChar opts
        tl = trimLines opts
        in interact $ unlines . map (trimIf tl fc) . fillTranspose fc . lines
