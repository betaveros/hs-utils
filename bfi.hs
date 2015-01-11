import Control.Monad.State
import Control.Applicative
import Data.Char
import Data.Int
import Data.List
import System.Environment

-- BF instructions all require access to a Tape of cells. We're going to say
-- that each cell is an Int. We represent a Tape with three fields: the current
-- cell, a list representing the cells on the left side, and a list
-- representing the cells on the right side. Since Haskell is lazy, we can make
-- both sides infinite.

data Tape a = Tape a [a] [a]

emptyTape :: (Num a) => Tape a
emptyTape = Tape 0 (repeat 0) (repeat 0)

-- We're going to use the State monad to implement BF actions, since they need
-- access to the tape. Since they also need access to input and output, we'll
-- need the IO monad too. So we'll use the StateT monad transformer wrapped
-- around IO.
type BFAction a = StateT (Tape a) IO ()

-- For people new to monad transformers (like myself, as I was writing this),
-- it's important to understand that StateT Tape IO is one monad!

-- The basic functionally pure manipulations on tapes:
addTape        :: (Num a) => a -> Tape a -> Tape a
shiftTapeLeft  :: (Num a) =>      Tape a -> Tape a
shiftTapeRight :: (Num a) =>      Tape a -> Tape a
setCell        :: (Num a) => a -> Tape a -> Tape a
getCell        :: (Num a) =>      Tape a ->      a

addTape n      (Tape c ls rs    ) = Tape (c + n) ls rs
shiftTapeLeft  (Tape c (l:ls) rs) = Tape l ls (c:rs)
shiftTapeRight (Tape c ls (r:rs)) = Tape r (c:ls) rs

setCell val (Tape _ ls rs) = Tape val ls rs
getCell (Tape c _ _) = c

-- Now we can implement the six atomic BF instructions easily.
bfiAtom :: (Num a, Integral a) => Char -> BFAction a

-- modify :: MonadState s m => (s -> s) -> m ()
-- Here, s = (Tape a) and m = (StateT (Tape a) IO)
-- It just runs a pure function on the state to modify it.
bfiAtom '+' = modify $ addTape 1
bfiAtom '-' = modify $ addTape (-1)
bfiAtom '<' = modify $ shiftTapeLeft
bfiAtom '>' = modify $ shiftTapeRight

-- The two IO actions!
-- get :: MonadState s m => m s (thus here, get :: State (Tape a) IO (Tape a))
-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- It lifts an unwrapped monad (IO a) to a wrapped monad (StateT Tape IO a)
-- putChar, after functional preprocessing, is the IO () being lifted
bfiAtom '.' = get >>= lift . putChar . chr . fromIntegral . getCell

-- getChar, after functional postprocessing (via fmap aka <$>) is the IO a
-- being lifted
bfiAtom ',' = (lift $ setCell . fromIntegral . ord <$> getChar) >>= modify

-- Other possible instructions are easy to implement, e.g.:
-- bfiAtom ':' = get >>= lift . print . getCell
-- bfiAtom ';' = (lift $ setCell <$> readLn) >>= modify
-- (note that these require adding (Read a, Show a) to the constraints of
-- bfiAtom and bfi)

-- Graciously ignore any character that isn't any of these
bfiAtom '[' = error "internal error: brackets shouldn't get through to bfiAtom"
bfiAtom ']' = error "internal error: brackets shouldn't get through to bfiAtom"
bfiAtom _   = return ()

-- Implement the loop semi-functionally. Takes a loop body (BFAction a) and
-- returns a loop that executes it as long as the current cell is 0.
bfLoop :: (Num a, Eq a) => BFAction a -> BFAction a
bfLoop s = do
    c <- get
    if getCell c == 0 then return () else s >> bfLoop s

-- Takes a string and cuts it around the first mismatched right bracket
-- (the mismatched right bracket is not included in either side)
spanBracket :: String -> (String, String)
spanBracket cs = f 0 cs
    where
        f :: Int -> String -> (String, String)
        f 0 (']':r) = ("", r)
        f d ( c :r) = let (cs', m) = f (d + dp c) r in (c:cs', m)
        f _ _ = error "spanBracket: no mismatched right bracket found"
        dp ']' = -1
        dp '[' = 1
        dp  _  = 0

-- Everything comes together!
bfi :: (Num a, Integral a, Eq a) => String -> BFAction a
bfi [] = return ()
bfi ('[':cs) = let (body, rest) = spanBracket cs in
    bfLoop (bfi body) >> bfi rest
bfi (']':_) = error "mismatched brackets"
bfi (c:cs) = bfiAtom c >> bfi cs

getOptions :: [String] -> [String]
getOptions = filter ((== '-') . head)

getFileArg :: [String] -> String
getFileArg args = case find ((/= '-') . head) args of
    Just x -> x
    Nothing -> error "no input file"

main :: IO ()
main = do
    args <- getArgs
    bfprog <- readFile $ getFileArg args
    let opts = getOptions args in
        if "-8" `elem` opts then
            evalStateT (bfi bfprog :: BFAction Int8) emptyTape
        else
            evalStateT (bfi bfprog :: BFAction Int) emptyTape
