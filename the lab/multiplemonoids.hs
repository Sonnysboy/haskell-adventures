{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (product)

class Product a where
    product :: a -> a -> a
    identity :: a
class Addition a where
    add :: a -> a -> a
instance Product Int where
    product = (*)
    identity = 1
instance (Product a) => Semigroup a where
    (<>) = product
instance (Product a) => Monoid a where
    mempty = identity

x :: (Show a, Monoid a) => a -> IO ()
x = print

main = x (4 :: Int)
