-- | Day 6 advent of code

-- | Check that all elements in a list is unique
allUnique :: Eq a => [a] -> Bool
allUnique l = case l of
  []     -> True
  (x:xs) -> x `notElem` xs && allUnique xs

-- | Solution to both questions.
solve :: String -> Int -> Maybe Int
solve s n = aux s 0
  where
    aux :: String -> Int -> Maybe Int
    aux (cur:rest) k = if allUnique (cur : take (n - 1) rest) then (Just $ k + n) else aux rest (k + 1)
    aux [] _         = Nothing

main :: IO ()
main = do
  contents <- readFile "data/day6.txt"
  print $ solve contents 4
  print $ solve contents 14
