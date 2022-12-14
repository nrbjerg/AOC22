import Data.List

type Pos = (Int, Int)

update :: [Pos] -> String -> [[Pos]]
update ps (dir:_:n) = aux dir (read n :: Int) ps
  where
    aux dir 0 ps = [ps]
    aux dir n ((x, y):rest) = let
        updated= case dir of
          'U' -> (x, y + 1)
          'D' -> (x, y - 1)
          'R' -> (x + 1, y)
          'L' -> (x - 1, y)
        updated_positions = (updated:aux' updated rest)
      in (updated_positions:aux dir (n - 1) updated_positions)
    aux' (x', y') ((x, y):rest)
      | abs(x - x') <= 1 && abs(y - y') <= 1 = (x, y) : aux' (x, y) rest
      | otherwise = (x + aux'' x' x, y + aux'' y' y) : aux' (x + aux'' x' x, y + aux'' y' y) rest
    aux' _ [] = []
    aux'' z' z
      | z == z' = 0
      | otherwise = if z' > z then 1 else -1

history :: [Pos] -> String -> [[Pos]]
history initial s = initial : aux initial (lines s)
  where
    aux ps (l:ls) = let
      history = update ps l
      in history ++ aux (head $ reverse history) ls
    aux _ [] = []

main :: IO ()
main = do
  contents <- readFile "data/day9.txt"
  print $ length $ nub . map (head . reverse) $ history (replicate 2 (0, 0)) contents
  print $ length $ nub . map (head . reverse) $ history (replicate 10 (0, 0)) contents
