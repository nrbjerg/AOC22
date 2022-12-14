-- | Day 2
import Data.Char (ord)

-- | Answer to first question.
solve :: String -> Int
solve c = sum $ map (\l -> ord (last l) + aux l - ord 'X' + 1) $ lines c
  where
    draws = ["A X", "B Y", "C Z"]
    winners = ["C X", "A Y", "B Z"]
    aux l
      | l `elem` winners = 6
      | l `elem` draws = 3
      | otherwise = 0

-- | Answer to second question.
solve' :: String -> Int
solve' c = sum $ map (\l -> aux (head l) (last l))  $ lines c
  where
    aux c1 c2 = let
      ls = case c2 of
        'X' -> [('A', 3), ('B', 1), ('C', 2)]
        'Y' -> [('A', 1), ('B', 2), ('C', 3)]
        'Z' -> [('A', 2), ('B', 3), ('C', 1)]
      in (snd . head $ filter (\x -> fst x == c1) ls) + (ord c2 - ord 'X') * 3

main :: IO ()
main = do
  contents <- readFile "data/day2.txt"
  print $ solve contents
  print $ solve' contents
