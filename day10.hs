sim :: String -> (Int -> Int -> [a]) -> [a]
sim instructions f = aux 1 (lines instructions) 1
  where
    aux reg (i:is) cycle
      | i == "noop" = f reg (cycle) ++ aux reg is (cycle + 1)
      | otherwise = f reg (cycle) ++ f reg (cycle + 1) ++ aux (reg + read (last $ words i) :: Int) is (cycle + 2)
    aux reg [] cycle = f reg cycle

display :: [Char] -> String
display [] = ""
display s = take 40 s ++ "\n" ++ (display $ drop 40 s)

main :: IO ()
main = do
  contents <- readFile "data/day10.txt"
  let
    arr = [20, 60, 100, 140, 180, 220]
    f reg cycle = [reg | cycle `elem` arr]
    f' reg cycle = if ((cycle `mod` 40) <= (reg + 2)) && ((cycle `mod` 40) >= (reg)) then "#" else "."
  print $ sum $ map(\(x, y) -> x * y) $ zip (sim contents f) arr
  putStrLn $ display $ sim contents f'
