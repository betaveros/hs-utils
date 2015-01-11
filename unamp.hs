{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.ByteString.Char8
    (Parser, takeWhile1, string, parseOnly)
import qualified Data.ByteString.Char8 as B
import Control.Applicative

unampParser :: Parser B.ByteString
unampParser = "&amp;"  ~> "&"  <|>
              "&lt;"   ~> "<"  <|>
              "&gt;"   ~> ">"  <|>
              "&quot;" ~> "\"" <|>
              takeWhile1 (/= '&')
    where s ~> c = string s >> return c

main :: IO ()
main = do
    c <- B.getContents
    case parseOnly (B.concat <$> many unampParser) c of
        Left msg -> error msg
        Right s -> B.putStrLn s
