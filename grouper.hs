import Data.List

centerpiece :: Int -> String
centerpiece n = let s = show n in s ++ [' ' | odd (n - length s)]

groupMark :: Int -> String
groupMark 0 = ""
groupMark 1 = "."
groupMark 2 = "()"
groupMark 3 = "[3]"
groupMark 4 = "[4 ]"
groupMark 5 = "[ 5 ]"
groupMark 6 = "[ 6  ]"
groupMark n =
    let
        c = centerpiece n
        h = (n - 6 - length c) `quot` 2
        s = replicate h '-'
    in concat ["[<", s, " ", c, " ", s, ">]"]

groupMarks :: String -> String
groupMarks = concatMap (groupMark . length) . group

groupMarkLines :: String -> [String]
groupMarkLines s = [s, groupMarks s]

main :: IO ()
main = interact $ unlines . concatMap groupMarkLines . lines
