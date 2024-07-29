module Product where
import Data.List


productArray :: [Integer] -> [Integer]
productArray xs = zipWith (\x i -> product (xs\\[x])) xs [0..]