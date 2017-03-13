import Data.List (intercalate)

escape :: Char -> String
escape '\'' = "\\'"
escape c = [c]

quote :: String -> String
quote s = "'" ++ concatMap escape s ++ "'"

sqlist :: [String] -> String
sqlist ss = "[" ++ intercalate "," (map quote ss) ++ "]"

main :: IO ()
main = interact $ sqlist . lines
