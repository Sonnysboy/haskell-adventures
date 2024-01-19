module PileOfCubes where


-- class 






findNb :: Integer -> Integer
findNb m = if (==m) $ fst ending then snd ending else -1
    where
        ending = head $ dropWhile (\(x, _) -> x < m) $ iterate (\(x,y) -> (x+((y+1)^3),y+1)) (1,1)


-- this DOES work but is too slow.

-- here is an O(1) version i didnt want to use maths but  had to.
{-

findNb :: Integer -> Integer
findNb m = if sumOfCubes == m then n else -1
  where
    n = floor $ ((sqrt (8 * sqrtM + 1) - 1) / 2)
    sqrtM = sqrt (fromIntegral m)
    sumOfCubes = (n * (n + 1) `div` 2)^2
-}

