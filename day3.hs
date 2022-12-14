-- | Day 3 of Advent of code

import Data.List
import Data.Char

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

-- | Answer to first question
solve :: String -> Int
solve s = sum $ map aux $ lines s
  where
    aux line = priority . head $ intersect (fst compartments) (snd compartments)
      where
        compartments = splitAt (div (length line) 2)  line

-- | Answer to second question
solve' :: String -> Int
solve' s = aux $ lines s
  where
    aux (x:y:z:zs) = (aux zs) + (priority . head $ intersect x $ intersect y z)
    aux _ = 0

main :: IO ()
main = do
  contents <- readFile "data/day3.txt"
  print $ solve contents
  print $ solve' contents
