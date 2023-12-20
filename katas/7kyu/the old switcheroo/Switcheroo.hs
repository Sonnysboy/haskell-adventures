module Switcheroo where

vowel2Index :: String -> String
vowel2Index str = foldr (\(i, x) acc -> (if isVowel x then show i else x : "") ++ acc) "" (zip [1 ..] str)
  where
    isVowel 'E' = True
    isVowel 'e' = True
    isVowel 'A' = True
    isVowel 'a' = True
    isVowel 'I' = True
    isVowel 'i' = True
    isVowel 'O' = True
    isVowel 'o' = True
    isVowel 'U' = True
    isVowel 'u' = True
    isVowel _ = False


-- better:
vowel2Index' :: String -> String
vowel2Index' = concat . zipWith replace [1..] where
    replace i x
        | x `elem` "aeiouAEIOU" = show i
        | otherwise = [x]
-- zipwith is cool
-- it's basically mapping each element of each list to f, i.e [f x y, f x1 y1, f x2 y2]