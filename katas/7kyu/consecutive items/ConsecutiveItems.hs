module ConsecutiveItems where


consecutive :: [Int] -> Int -> Int -> Bool
consecutive list x y = any (\(a, b) -> ((a == x) && (b == y)) || ((a == y) && (b == x))) $ zip list $ tail list

