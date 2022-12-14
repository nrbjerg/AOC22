-- | Day 4 of AOC

import Data.List (intersect)

split' :: Char -> String -> [String]
split' c s = aux c s ""
  where
    aux c (s:ss) p = if c == s then [p, ss] else aux c ss $ p ++ [s]

parse :: String -> [[[Int]]]
parse s = map (map (aux . split' '-') . split' ',') $ lines s where
  aux l = [read (head l) :: Int, read (head $ tail l) :: Int]

-- | Solution to question 1
solve :: String -> Int
solve s = sum $ map aux $ parse s
  where
    aux (x:y:_) = if (head x <= head y && x !! 1 >= y !! 1) ||
                     (head x >= head y && x !! 1 <= y !! 1)  then 1 else 0

-- | Solution to question 2
solve' :: String -> Int
solve' s = aux $ parse s
  where
    aux ((x:y:_):ns)
      | intersect [head x .. x !! 1] [head y .. y !! 1] /= [] = 1 + aux ns
      | otherwise = 0 + aux ns
    aux [] = 0


main :: IO ()
main = do
  contents <- readFile "data/day4.txt"

  print $ solve contents
  print $ solve' contents
