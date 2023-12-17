
-- shit's getting serious now

-- Anonymous functions

-- suppose we have
gt100 :: Integer -> Bool
gt100 x =  x >100

greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs


-- we can use a lambda for this
greaterThan100_2 xs = filter (\x -> x > 100) xs

-- apparently the \ is supposed to look like a lambda with the short leg missing lmao

-- lambdas work inline too
example :: [Integer]
example = (\x y z -> [x, 2*y, z*3]) 1 2 3

-- introducing operator sections
-- we can also write our greater than 100 filters like this
greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs

-- OPERATOR SECTION definition
-- if ? is an operator, then (?y) is equivalent to \x -> x ? y and (y?) is equivalent to \x -> y ? x
-- as in this is partially applying an operator to one of its two arguments, and obtain a function that takes 1 argument.
-- (>100) 102 translates to 102 > 100
-- (100>) 102 translates to 100 > 102
-- map (*6) [1..5] = [6, 12, 18, 24, 30]

-- function composition
comp :: (b -> c) -> (a -> b) -> (a -> c)
comp f g = \x -> f (g x)
-- we basically just rewrote `.` lets use it now
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

-- we can write this point-free like this
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100


-- the revelation : ALL FUNCTIONS TAKE 1 ARGUMENT
f :: Integer -> Integer -> Integer
f x y = 2 * x + y -- this actually takes 1 integer and outputs a function of type Integer -> Integer that returns the final answer


-- function arrows associate to the right
-- W -> X -> Y -> Z is equivalent to W -> (X -> (Y -> Z))
-- but function application is left-associative
-- f 3 2 is short for (f 3) 2  but since functions associate to the left we can shorthand it to f 3 2
-- which is just an illusion for multi-argument functions

-- and for lambdas:
-- \x y z -> ...
-- is sugar for (\x -> (\y -> (\z -> ...))) and now my head hurts