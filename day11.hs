data Monkey = Monkey {items :: [Int], op :: (Int -> Int), test :: (Int -> Bool), throw_to :: (Int)}


throw :: Monkey -> [Moneky] -> Monkey
throw (Monkey levels op test (i, j)) monkeys = aux levels op tests
  where
    aux levels op tests =

sim :: [Monkey] -> Int -> [Monkey]
sim monkeys 0 = monkeys
sim monkeys n = sim (aux monkeys monkeys) n - 1
  where
    aux [] new_monkeys _ = new_monkeys
    aux (m:monkeys) new_monkeys n = throw m monkeys new_monkeys
    throw (Monkey items op test throw_to) mon mon'

main :: IO ()
main = do
  print $ sim [
    Monkey [59, 74, 65, 86] (\x -> x * 19) (\x -> (x `mod` 7) == 0) (6, 2),
    Monkey [62, 84, 72, 91, 68, 78, 51] (\x -> x + 1) (\x -> (x `mod` 2) == 0) (2, 0),
    Monkey [78, 84, 96] (\x -> x + 8) (\x -> (x `mod` 19)) (6, 5),
    Monkey [97, 86] (\x -> x * x) (\x -> (x `mod` 3) == 0) (1, 0),
    Monkey [50] (\x -> x + 6) (\x -> (x `mod` 13) == 0) (3, 1),
    Monkey [73, 65, 69, 65, 51] (\x -> x * 17) (\x -> (x `mod` 11) == 0) (4, 7),
    Monkey [69, 82, 97, 93, 82, 84, 58, 63] (\x -> x + 5) (\x -> (x `mod` 5) == 0) (5, 7),
    Monkey [81, 78, 82, 76, 79, 80] (\x -> x + 3) (\x -> (x `mod` 17) == 0) (3, 4)
  ]
