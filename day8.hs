import Prelude hiding (Left, Right)
import Data.Char

parse :: String -> [[Int]]
parse s = map (map digitToInt) $ lines s

get :: (Int, Int) -> [[Int]] -> Int
get (i, j) mat = (mat !! i) !! j

data Dir = Up | Down | Left | Right

elementsInDir :: Dir -> (Int, Int) -> [[Int]] -> [Int]
elementsInDir d (i, j) mat = case d of
  Up -> reverse $ [get (k, j) mat | k <- [0 .. i - 1] ]
  Down -> [get (k, j) mat | k <- [i + 1 .. length mat - 1] ]
  Left -> reverse $ take j (mat !! i)
  Right -> reverse $ take (length mat - 1 - j) $ reverse (mat !! i)

solve :: String -> Int
solve s = sum $ [sum $ [aux (i, j) | j <- [0 .. length (head mat) - 1] ] | i <- [0 .. length mat - 1]]
  where
    mat = parse s
    isVisible (i, j) = foldl (||) False $ map (\dir -> get (i, j) mat > (maximum $ elementsInDir dir (i, j) mat)) [Up, Down, Left, Right]
    aux (i, j)
      | i == 0 || j == 0 || i == length mat - 1 || j == length (head mat) - 1 || isVisible (i, j) = 1
      | otherwise = 0

solve' :: String -> Int
solve' s = maximum $ [ maximum $ [aux (i, j) | j <- [0 .. length (head mat) - 1]] | i <- [0 .. length mat - 1]]
  where
    mat = parse s
    aux' :: [Int] -> Int -> Int -> Int
    aux' (cur:rest) elm l
      | cur < elm = aux' rest elm (l + 1)
      | otherwise = l + 1
    aux' [] _ l = l
    aux (i, j) = product $ map (\dir -> aux' (elementsInDir dir (i, j) mat) (get (i, j) mat) 0) [Up, Down, Left, Right]

main :: IO ()
main = do
  contents <- readFile "data/day8_test.txt"
  print $ solve contents
  print $ solve' contents
