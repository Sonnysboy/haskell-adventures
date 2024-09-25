 -- boom yep this is exactly how it's defined in base. exxcept in base there's an extend (and unjoin is based off of it and is called duplicate, and unreturn is called extract)
class Functor m => Comonad m where
    unreturn :: m a -> a

    unjoin :: m a -> m (m a)
    unjoin = extend id

    extend :: (m a -> b) -> m a -> m b
    extend f = fmap f . unjoin

instance Comonad [] where
    unreturn :: [a] -> a
    unreturn [x] = x
    unreturn (x:xs) = x
    unjoin :: [a] -> [[a]]
    unjoin x = [x]