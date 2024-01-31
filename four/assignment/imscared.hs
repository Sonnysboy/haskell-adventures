import Data.Bits qualified as Bits
import Data.List
import Data.Maybe (listToMaybe)

{-
Exercise 1: Wholemeal programming
Reimplement each of the following functions in a more idiomatic
Haskell style. Use wholemeal programming practices, breaking each
function into a pipeline of incremental transformations to an entire
data structure. Name your functions fun1’ and fun2’ respectively.
1. fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
\| even x = (x - 2) * fun1 xs
\| otherwise = fun1 xs
2. fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
\| otherwise = fun2 (3 * n + 1)
Hint: For this problem you may wish to use the functions iterate
and takeWhile. Look them up in the Prelude documentation to see
what they do.
-}
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> acc * (x - 2)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- fun2' :: Int -> Int
-- fun2' n = sum . filter even . takeWhile (/= 1) $ iterate (\x ->
-- (fromEnum (even x)) * (x `div` 2) + (fromEnum (odd x) * (3 * x + 1)) ) n heres a way to do it without the if statement lol

{-
Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains. For example,
xor [False, True, False] == True
xor [False, True, False, False, True] == False
Your solution must be implemented using a fold.
-}

xor :: [Bool] -> Bool
xor = foldr Bits.xor False -- lol

{-
Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.
-}

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\c acc -> f c : acc) [] xs 

{-
Exercise 4: Finding primes
Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
of_Sundaram
ing function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram = ...
To give you some help, below is a function to compute the Carte-
sian product of two lists. This is similar to zip, but it produces all
possible pairs instead of matching up the list elements. For example,
cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
It’s written using a list comprehension, which we haven’t talked about
in class (but feel free to research them).
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys

-}
-- nope im using a soe and doing it the cool way
primes :: Integer -> [Integer]
primes n =
  let prime x = x /= 2 && not (any (\i -> (x `mod` i) == 0) [2 .. x - 1]) in filter prime [1 .. 2 * n + 1]

-- equipment get!! let in, aka lemme take a break and declare a variable rq