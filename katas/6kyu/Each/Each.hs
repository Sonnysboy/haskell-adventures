module Each where

each :: Int -> [Int] -> [Int]
each n xs
  | n == 0 = []
  | n < 0 = each ((-1) * n) (reverse xs)
  | otherwise = filterIndexed (\x i -> (i+1) `mod` n == 0) xs


filterIndexed :: (a -> Int -> Bool) -> [a] -> [a]
filterIndexed p xs = [x|(x,i) <- zip xs [0..], p x i]

-- I could've used `negate n` instead of (-1 * n)