-- | -- | Day 4 of AOC

import Data.List (intersect)

-- NOTE: https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
-- | Knuth Morris Pratt algorithm, for string searching. (Note that this is a generalized version, taking any datatype equiped with the equality trait.)
kmpSearch :: Eq a => [a] -> [a] -> [Int]
kmpSearch pat str = let
  aux pat (cur:rest) ocur k j
    | cur == (pat !! k) = if k == length pat - 1 then
                             aux pat rest ((j - length pat + 1) : ocur) 0 (j + 1)
                           else
                             aux pat rest ocur (k + 1) (j + 1)
    | otherwise         = aux pat rest ocur 0 (j + 1)
  aux _ [] ocur _ _ = ocur
  in reverse $ aux pat str [] 0 0

-- | Another version of the Knuth Morrish Pratt, which only returns the first occurence
kmpSearch' :: Eq a => [a] -> [a] -> Maybe Int
kmpSearch' [] _ = Nothing
kmpSearch' _ [] = Nothing
kmpSearch' pat str = let
  aux pat (cur:rest) k j
    | k == length pat = Just (- k) -- Im not totally sure why this has to be there.
    | cur == (pat !! k)   = if k == length pat - 1 then
                              Just (j - k)
                            else
                              aux pat rest (k + 1) (j + 1)
    | otherwise           = aux pat rest 0 (j + 1)
  aux _ [] _ _ = Nothing
  in aux pat str 0 0

-- | Split on based uppon kmpSearch.
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn pat str = aux pat str []
  where
    aux pat str reg = case kmpSearch' pat str of
      Just i  -> aux pat (reverse $ take (length str - i - length pat) $ reverse str) (reg ++ [take i str])
      Nothing -> if str == [] then reg else reg ++ [str]

-- | Parse the state
state :: String -> [[Char]]
state st = let
  ls = lines st
  in
    aux ls [[] | _ <- [1..9]] 0
  where
    aux (l:ls) cols i
      | i == 9    = aux ls cols 0
      | otherwise = let item  = l !! (4 * i + 1)
                        prev  = take i cols
                        later = reverse $ take (9 - i - 1) $ reverse cols
                    in if item /= ' ' then
                         aux (l:ls)  (prev ++ [(cols !! i) ++ [item]] ++ later) (i + 1)
                       else
                         aux (l:ls) cols (i + 1)
    aux [] cols _ = cols

-- | Parse everything
parse :: String -> ([[Char]], [[Int]])
parse s = aux $ splitOn "\n\n" s
  where
    moves ms = [(\ws -> [read (ws !! 1), read (ws !! 3) - 1, read (ws !! 5) - 1]) $ words l | l <- lines ms]
    state' st = state $ unlines $ take (length (lines st) - 1) $ lines st
    aux (x:y:_) = (state' x, moves y)


-- | Solution to question 1
solve :: String -> [Char]
solve s = let
  p = parse s
  in map (head) $ aux (fst p) (snd p) where
    aux st (m:ms) = let n = m !! 0
                        i = m !! 1
                        j = m !! 2
                    in aux [aux' st k i j n | k <- [0..8] ] ms
                      where
                        aux' st cur from to n
                          | cur == from = drop n $ st !! cur
                          | cur == to   = reverse $ reverse (st !! cur) ++ take n (st !! from)
                          | otherwise   = st !! cur
    aux st [] = st

-- | Solution to question 2
solve' :: String -> [Char]
solve' s = let
  p = parse s
  in map (head) $ aux (fst p) (snd p) where
    aux st (m:ms) = let n = m !! 0
                        i = m !! 1
                        j = m !! 2
                    in aux [aux' st k i j n | k <- [0..8] ] ms
                      where
                        aux' st cur from to n
                          | cur == from = drop n $ st !! cur
                          | cur == to   = take n (st !! from) ++ (st !! cur)
                          | otherwise   = st !! cur
    aux st [] = st

-- | Run the thing
main :: IO ()
main = do
  contents <- readFile "data/day5.txt"
  print $ parse contents
  print $ solve contents
  print $ solve' contents

