module Narcissistic where


narcissistic :: (Integral n) => n -> Bool
narcissistic n = toInteger n == sum (map (^l) digits)
    where
        d = map (read . return) . show
        digits = d $ toInteger n
        l = length digits
        forall 