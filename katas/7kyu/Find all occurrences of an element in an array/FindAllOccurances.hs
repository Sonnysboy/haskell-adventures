module FindAllOccurences (findAll) where
import Data.List (elemIndices)

findAll :: [Int] -> Int -> [Int]
findAll [] _ = []
findAll xs n = filter (\x -> (xs!!x) == n) [0..length xs - 1]

-- top:


findAll' :: [Int] -> Int -> [Int]
findAll' = flip elemIndices
{-

The elemIndices function extends elemIndex , by returning the indices of all elements equal to the query element, in ascending order.

>>> elemIndices 'o' "Hello World"
[4,7]
There's also:

findIndices (== n) xs
-}
