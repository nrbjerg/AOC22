import Data.Char
import Prelude hiding (lookup)
import Data.Maybe
import Data.List (sort)

-- | From day 5
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

-- | Hashmap implementation inspired by: https://gist.github.com/davenportw15/acb236df3b02621be482
data HashMap k v = EmptyMap | Map (k, v) (HashMap k v) (HashMap k v) deriving (Read, Show)

insert :: (Ord k) => (k, v) -> HashMap k v -> HashMap k v
insert pair m = case m of
  EmptyMap -> Map pair EmptyMap EmptyMap
  (Map stored left right) -> case fst pair `compare` fst stored of
      EQ -> Map pair left right
      GT -> Map stored left (insert pair right)
      LT -> Map stored (insert pair left) right

lookup' :: (Ord k) => k -> HashMap k v -> Maybe v
lookup' lookup_key m = case m of
  EmptyMap -> Nothing
  (Map (key, val) left right) -> case lookup_key `compare` key of
    EQ -> Just val
    GT -> lookup' lookup_key right
    LT -> lookup' lookup_key left

keys :: (Ord k) => HashMap k v -> [k]
keys m = case m of
  EmptyMap -> []
  (Map (key, _) left right) -> (key:keys left) ++ keys right

-- | Solutions to question 1 and 2
removeBackslash :: String -> String
removeBackslash dir = take (length dir - (kmpSearch "/" $ reverse dir) !! 1) dir

convertToHashMap :: String -> HashMap String Int
convertToHashMap s = aux (insert ("/", 0) EmptyMap)"/" $ map words $ lines s
  where
    aux m dir (l:ls)
      | head l == "$" && l !! 1 == "cd" && l !! 2 == ".." = aux m (removeBackslash dir) ls
      | head l == "$" && l !! 1 == "cd" = let
          dir' = dir ++ l !! 2 ++ "/"
          in aux (insert (dir', 0) m) dir' ls
      | (isDigit . head . head) l = aux (insert (dir ++ l !! 1, read $ head l) m) dir ls
      | otherwise = aux m dir ls
    aux m _ [] = m

isFile :: String -> Bool
isFile path = (head $ reverse path) /= '/'

isDir :: String -> Bool
isDir path = head (reverse path) == '/'

checkForMatch :: String -> String -> Bool
checkForMatch (c:res) (c':res') = c == c' && checkForMatch res res'
checkForMatch [] _ = True
checkForMatch _ [] = False

computeSizes :: HashMap String Int -> HashMap String Int
computeSizes m = aux m $ keys m where
  size m key = sum $ mapMaybe (\path -> lookup' path m) $ filter (\path -> isFile path && checkForMatch key path) $ keys m
  aux m (path:rest)
    | isDir path = aux (insert (path, size m path) m) rest
    | otherwise = aux m rest
  aux m [] = m

solve :: HashMap String Int -> Int
solve m = sum $ filter (\s -> s <= 100000) $ mapMaybe (\path -> lookup' path m) $ filter isDir $ keys m

solve' :: HashMap String Int -> (Int, Int, Int, Int)
solve' m = let
  used = case lookup' "/" m of
    Just n -> n
    Nothing -> 0
  minimum_to_delete = 30000000 - (70000000 - used)
  sizes =  mapMaybe (\path -> lookup' path m) $ filter isDir $ keys m
  in (used, minimum_to_delete, 70000000 - used, minimum $ filter (\s -> s >= minimum_to_delete) sizes)


main :: IO ()
main = do
  hashMap <- computeSizes <$> convertToHashMap <$> readFile "data/day7_thomas.txt"
  --hashMap <- computeSizes <$> convertToHashMap <$> readFile "data/day7_test.txt"
  print $ solve hashMap
  print $ solve' hashMap
