module Kata where
import Data.List (intercalate)

expandedForm :: Int -> String
expandedForm n = take (length res - 3) res
  where
    digits = map (read . return) . show $ n
    digitL = length digits
    e = filter (\(i, x) -> 0 /= x) $ zip [0 ..] digits
    res = foldr (\(i, x) acc -> (show (x * (10 ^ ((digitL - i) - 1))) ++ " + ") ++ acc) "" e


-- and of course the best solution:
expandedForm' :: Int -> String
expandedForm' = intercalate " + " . map(\(n, c) ->  c : replicate n '0' ) . reverse . filter ((/='0') . snd) . zip [0..] . reverse . show