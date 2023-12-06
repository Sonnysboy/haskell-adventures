module Validator where
toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 = [0]
  | n < 10 = [n]
  | otherwise = toDigits (n `div` 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 = [0]
  | n < 10 = [n]
  | otherwise = mod n 10 : toDigits (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (aux (reverse list))
  where
	aux [] = []
	aux [x] = [x] -- one-element list is itself
	aux (x : (y : zs)) = x : (y * 2) : aux zs -- [x,y,...zs] list starting with x followed by a list starting with y

-- convert all numbers in a list into their digits and then sum them
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x : zs) = sum (toDigits x) + sumDigits zs



-- sum of all the digits of the numbers of the list
rep :: [Integer] -> [Integer]
rep list = [sumDigits (toDigits n) | n <- list]


{-
	Double the value of every second digit starting from the right.
	Sum this new list
	If (this sum) mod 10 == 0, then its a valid card

-}
validate :: Integer -> Bool
validate card = (mod (sum (rep (doubleEveryOther (toDigits card)))) 10) == 0
