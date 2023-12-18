sumMul :: Int -> Int -> Maybe Int
sumMul n m
  | m <= n = Nothing
  | otherwise = Just (sum (takeWhile (< m) $ iterate (+ n) n))
