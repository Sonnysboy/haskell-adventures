

{-
Exercise 1
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{-

    Now it wants me to do it infinite-style.
-}
fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

fibs2 :: [Integer]
fibs2 = map fib [0..]

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream value _) = [value]


