{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Prelude hiding (Functor)
import Control.Applicative
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

class Eq a => Group a where

    binaryOperation :: a -> a -> a -- A x A -> A such that forall x y. binaryOperation(x,y) = binaryOperation(y,x)

    identity :: a  -- A such that forall x. binaryOperation(x, identity) = x

    inv :: a -> a -- A such that forall x a. binaryOperation(x, inv(a)) = inv = binaryOperation(inv, x(a))

instance Group Int where
    binaryOperation = (+)
    identity = 0
    inv a = -a
instance Eq a => Group (Maybe a) where
  binaryOperation :: Eq a => Maybe a -> Maybe a -> Maybe a
  binaryOperation = (<|>)
  identity :: Eq a => Maybe a
  identity = Nothing
  inv :: Eq a => Maybe a -> Maybe a
  inv = const Nothing

ker :: forall g g'. (Group g, Group g') => (g -> g') -> [g] -> [g]
ker f x = filter (\x -> f x == identity) x

f :: Maybe Int -> Int
f Nothing = 0
f (Just x) = x


x :: [Maybe Int]
x = ker f (map Just [-40..10] ++ [Nothing | _ <- [0..10]])