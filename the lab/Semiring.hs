class Semiring a where
    zero :: a
    one :: a
    add :: a -> a -> a
    prod :: a -> a -> a

class Semiring a => Ring a where
    inverse :: a

instance Semiring Int where
    zero = 0
    one = 1
    add = (+)
    prod = (*)

