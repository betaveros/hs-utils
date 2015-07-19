{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.ByteString.Char8
    (Parser, takeWhile1, char, parseOnly)
import qualified Data.ByteString.Char8 as B
import Control.Applicative

ampParser :: Parser B.ByteString
ampParser = '&' ~> "&amp;" <|>
            '<' ~> "&lt;"  <|>
            '>' ~> "&gt;"  <|>
            takeWhile1 (`B.notElem` "&<>")
    where c ~> s = char c >> return s

main :: IO ()
main = do
    c <- B.getContents
    case parseOnly (B.concat <$> many ampParser) c of
        Left msg -> error msg
        Right s -> B.putStrLn s
