module Multiples where

multiples :: Int -> Double -> [Double]
multiples amt x = [x * fromIntegral a | a <- [1..amt]]
