
-- let's say we have a function
-- f :: a -> a -> a
-- We have to make sure that this function works for any type. The only way I can think of to make that work is this:
f :: a -> a -> a
f x y = x -- or f x y = y or nothing 
-- haskell has NO instanceof function or anything like that. all types are erased at runtimeo

-- *** parametrized functions ***

data Foo = F Int | G Char

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _ == _ = False
    x /= y = not $ x == y

    
class Listable a  where
    toList :: a -> [Int]
instance Listable Int where
    toList x = [x] -- or [x], return is just list monad stuff

instance Listable Bool where
    toList True = [1]
    toList False= [0]
-- we can have multiple type class constraints at once
sumL :: Listable a => a -> Int
sumL = sum . toList
foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sumL x == sumL y || x < y 