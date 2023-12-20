module War where


alphabetWar :: String -> String
alphabetWar string = case compare (sum $ map (\x -> case x of 
    'w' -> -4
    'p' -> -3
    'b' -> -2
    's' -> -1
    'm' -> 4
    'q' -> 3
    'd' -> 2
    'z' -> 1
    _ -> 0 ) string) 0 of
            GT -> "Right side wins!"
            LT -> "Left side wins!"
            _ -> "Let's fight again!"


-- best solution:
alphabetWar' :: String -> String
alphabetWar' s
  | result s > 0 = "Right side wins!"
  | result s < 0 = "Left side wins!"
  | otherwise = "Let's fight again!"
  where
    power 'm' = 4
    power 'q' = 3
    power 'd' = 2
    power 'z' = 1
    power 's' = -1
    power 'b' = -2
    power 'p' = -3
    power 'w' = -4
    power _ = 0
    result = sum . map power