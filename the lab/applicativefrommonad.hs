-- Write a definition of (<*>) using (>>=) and fmap. Do not use do-notation.
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- fmap :: Functor f => (a -> b) -> f a -> f b

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) f x = f >>= \y -> fmap y x