{-# LANGUAGE OverloadedStrings, ImplicitParams #-}
import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
    (Parser, takeWhile, takeWhile1
    , string, choice, parseOnly, endOfInput, manyTill, (<?>))
import Control.Applicative ((<$>), (<|>))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, hPutBuilder)
import Data.Char (isDigit, isSpace)
import Data.Monoid (mconcat)
import System.Environment (getArgs)
import System.IO (stdout)
import qualified Data.ByteString.Char8 as B

data Start = Foot | Ref | Note | Plain

vstring :: ByteString -> Parser ()
vstring s = void $ string s

footStarter :: Parser Start
footStarter = string "<li id=\"fn:" >> return Foot
footParser :: (?slug :: ByteString) => Parser ByteString
footParser = (<?> "footParser") $ do
    ds <- takeWhile1 isDigit
    vstring "\">"
    ss <- takeWhile isSpace
    let lis = ["<li id=\"", ?slug, "foot", ds, "\">", ss]
    let ans = ["<a href=\"#", ?slug, "note", ds, "\">^</a> "]
    (vstring "<p>" >> return (mconcat (lis ++ ["<p>"] ++ ans)))
        <|> return (mconcat (lis ++ ["<p>"] ++ ans ++ ["</p>"]))

-- Possible future work: this actually leaves a stray space before it.
-- Do we want to handle that?
refStarter :: Parser Start
refStarter = string "<a href=\"#fnref:" >> return Ref
refParser :: Parser ByteString
refParser = (<?> "refParser") $ do
    void $ takeWhile1 isDigit
    vstring "\" class=\"reversefootnote\">&#8617;</a>"
    return ""

noteStarter :: Parser Start
noteStarter = string "<sup id=\"fnref:" >> return Note
noteParser :: (?slug :: ByteString) => Parser ByteString
noteParser = (<?> "noteParser") $ do
    ds <- takeWhile1 isDigit
    vstring "\"><a href=\"#fn:"
    vstring ds
    vstring "\" class=\"footnote\">"
    return $ mconcat ["<sup id=\"", ?slug, "note", ds, "\"><a href=\"#", ?slug, "foot", ds, "\">"]

kdfootParser :: (?slug :: ByteString) => Parser ByteString
kdfootParser = do
    start <- choice [footStarter, refStarter, noteStarter, return Plain]
    case start of
        Foot -> footParser
        Ref -> refParser
        Note -> noteParser
        Plain -> string "<" <|> takeWhile1 (/= '<')

main :: IO ()
main = do
    args <- getArgs
    let ?slug = case args of
            [s] -> B.pack s
            _ -> error "Exactly one argument (the footnote slug), please"
    c <- B.getContents
    case parseOnly (mconcat . map byteString <$> manyTill kdfootParser endOfInput) c of
        Left msg -> error msg
        Right b -> hPutBuilder stdout b
