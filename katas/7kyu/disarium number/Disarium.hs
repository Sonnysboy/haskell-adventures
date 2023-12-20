module Disarium where

disariumNumber :: Int -> String
disariumNumber 1 = "Disarium !!"
disariumNumber n = if foldr (\(x, y) acc -> acc + (y ^ (x + 1))) 0 (zip [0 ..] (digits n)) == n then "Disarium !!" else "Not !!"
  where
    digits = map (read . return) . show

-- we can mimic python's enumerate function by using zip  [0..]
-- i.e zip [0..] [1,2,3,4] = [(0,1),(1,2),(2,3),(3,4)]