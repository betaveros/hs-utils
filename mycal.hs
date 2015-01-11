#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Data.Char (chr)
import Data.Time.Calendar (Day, toGregorian, fromGregorian, isLeapYear)
import Data.Time.LocalTime (ZonedTime, localDay, zonedTimeToLocalTime, getZonedTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar.MonthDay (monthLength)
import Data.List.Split (chunksOf)

cyan :: String
cyan = chr 27 : "[0;36m"
nocolor :: String
nocolor = chr 27 : "[0m"

extractDay :: ZonedTime -> Day
extractDay = localDay . zonedTimeToLocalTime

data DayCell = Empty | NormalCell Int | TodayCell Int

lengthOfMonth :: Day -> Int
lengthOfMonth day = let (y, m, _) = toGregorian day in
    monthLength (isLeapYear y) m

-- Note, I want Sunday -> 0 instead of 7 as toWeekday gives me.
toWeekday :: Day -> Int
toWeekday = (`mod` 7) . ( \(_,_,wd) -> wd ) . toWeekDate

weekDayOfFirst :: Day -> Int
weekDayOfFirst day = let (y, m, _) = toGregorian day in
    toWeekday $ fromGregorian y m 1

dayListInMonth :: Day -> [Int]
dayListInMonth = enumFromTo 1 . lengthOfMonth

toDayCell :: Day -> Int -> DayCell
toDayCell today d
    | todayd == d = TodayCell d
    | otherwise   = NormalCell d
    where (_,_,todayd) = toGregorian today

weekListInMonth :: Day -> [[DayCell]]
weekListInMonth day =
    chunksOf 7 $ replicate (weekDayOfFirst day) Empty ++ map (toDayCell day) (dayListInMonth day)

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

printCalendar :: Day -> IO ()
printCalendar day = do
    putStrLn $ replicate 8 ' ' ++ monthLabel day
    putStrLn weekLabel
    mapM_ (putStrLn . weekToLine) $ weekListInMonth day

main :: IO ()
main = fmap extractDay getZonedTime >>= printCalendar
