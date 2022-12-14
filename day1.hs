-- | Day 1 of advent of code
import Data.List

parse :: String -> [[Int]]
parse = aux [] [] . lines
  where
    aux y z (x:xs) = if x == "" then aux (z:y) [] xs else aux y ((read x :: Int) :z) xs
    aux y z [] = if null z then y else z:y


main :: IO ()
main = do
  contents <- readFile "data/day1.txt"
  -- Question 1
  print $ maximum . map sum $ parse contents

  -- Question 2
  print $ sum . take 3 $ reverse . sort $ map sum $ parse contents
