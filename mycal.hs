#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Char (chr)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, isLeapYear, addGregorianMonthsClip)
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (ZonedTime, localDay, zonedTimeToLocalTime, getZonedTime)
import System.Environment (getArgs)
import Text.Read (readMaybe)

cyan :: String
cyan = chr 27 : "[0;36m"
nocolor :: String
nocolor = chr 27 : "[0m"

extractDay :: ZonedTime -> Day
extractDay = localDay . zonedTimeToLocalTime

data DayCell = Empty | NormalCell Int | TodayCell Int

-- Note, I want Sunday -> 0 instead of 7 as toWeekday gives me.
toWeekday :: Day -> Int
toWeekday = (`mod` 7) . ( \(_,_,wd) -> wd ) . toWeekDate

toDayCell :: Day -> Int -> DayCell
toDayCell today d
    | todayd == d = TodayCell d
    | otherwise   = NormalCell d
    where (_,_,todayd) = toGregorian today

makeWeekList :: Integer -> Int -> (Int -> DayCell) -> [[DayCell]]
makeWeekList y m f = 
    chunksOf 7 $ replicate (toWeekday $ fromGregorian y m 1) Empty ++ map f [1 .. monthLength (isLeapYear y) m]

weekListInMonth :: Bool -> Day -> [[DayCell]]
weekListInMonth hi day = let (y, m, _) = toGregorian day in
    makeWeekList y m (if hi then toDayCell day else NormalCell)

pad :: Int -> String -> String
pad len s = replicate (len - length s) ' ' ++ s

padDay :: Int -> String
padDay d = pad 2 $ show d

showCell :: DayCell -> String
showCell Empty = "  "
showCell (NormalCell d) = padDay d
showCell (TodayCell d) = cyan ++ padDay d ++ nocolor

showMaybeCell :: Maybe DayCell -> String
showMaybeCell = maybe "" showCell

showCellDelimiter :: Maybe DayCell -> Maybe DayCell -> String
showCellDelimiter (Just (TodayCell _)) _ = "]"
showCellDelimiter _ (Just (TodayCell _)) = "["
showCellDelimiter _ _ = " "

showCellAndDelimiter :: Maybe DayCell -> Maybe DayCell -> String
showCellAndDelimiter a b = showMaybeCell a ++ showCellDelimiter a b

weekToLine :: [DayCell] -> String
weekToLine wk = concat $ uncurry (zipWith showCellAndDelimiter) $ ((Nothing :) &&& (++ [Nothing])) $ map Just wk

weekLabel :: String
weekLabel = " Su Mo Tu We Th Fr Sa "

monthLabel :: Day -> String
monthLabel day = let (y,m,_) = toGregorian day in show y ++ "/" ++ show m

printDays :: Bool -> Day -> IO ()
printDays hi day =
    mapM_ (putStrLn . weekToLine) $ weekListInMonth hi day

printCalendar :: Integer -> Day -> IO ()
printCalendar futureMonths day = do
    putStrLn $ replicate 8 ' ' ++ monthLabel day
    putStrLn weekLabel
    printDays True day
    forM_ [1..futureMonths] $ \i ->
        printDays False (addGregorianMonthsClip i day)

main :: IO ()
main = do
    args <- getArgs
    let n = case args of
            [] -> 0
            [x] -> fromMaybe (error "could not parse args") $ readMaybe x
            _ -> error "0 or 1 args"
    fmap extractDay getZonedTime >>= printCalendar n
