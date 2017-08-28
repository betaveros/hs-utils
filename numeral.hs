{-# LANGUAGE OverloadedStrings #-}
import Text.Numeral.Grammar (defaultInflection)
import qualified Text.Numeral.Language.ENG as ENG
import System.Environment
import qualified Data.Text.IO as T
import Data.Maybe

main :: IO ()
main = do
    [n] <- getArgs
    T.putStrLn . fromMaybe "Failed" $ ENG.us_cardinal defaultInflection (read n :: Integer)
