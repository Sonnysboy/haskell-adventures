module Kata where
import Data.Function
import Data.Char

-- compare function is part of Prelude => compare' is used instead
compare' :: Maybe String -> Maybe String -> Bool
compare' s1 s2 = (grabValue <$> s1) == (grabValue <$> s2)
    where
        grabValue :: String -> Int
        grabValue s
            | not $ all isAlpha s = 0
            | otherwise = sum $ map (ord . toUpper) s