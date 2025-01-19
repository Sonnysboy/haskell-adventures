{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Counting where

import Control.Arrow
import Control.Monad (filterM)
import Data.Coerce (coerce)
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Kind (Type)
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Void

data Nat = Z | S Nat deriving (Show, Eq, Ord)

nat :: a -> (Nat -> a) -> Nat -> a
nat a _ Z = a
nat _ aa (S n) = aa n

iterNat :: (a -> a) -> Nat -> (a -> a)
iterNat _ Z = id
iterNat aa (S n) = aa . iterNat aa n

natUnderflow :: String
natUnderflow = "Nat is non-negative"

instance Num Nat where
  (+) = iterNat S
  a * b = iterNat (b +) a Z
  a - Z = a
  Z - b = error natUnderflow
  S a - S b = a - b
  abs = id
  signum Z = Z
  signum (S _) = S Z
  fromInteger x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ fromInteger $ x - 1

instance Enum Nat where
  toEnum x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ toEnum $ x - 1
  fromEnum x = iterNat (+ 1) x 0

instance Real Nat where toRational = toRational . fromEnum

instance Integral Nat where
  quotRem a Z = error "divide by zero"
  quotRem a b = until ((< b) . snd) (S *** subtract b) (Z, a)
  divMod = quotRem
  toInteger n = iterNat (+ 1) n 0

newtype Count x = Count {getCount :: Nat} deriving (Show, Eq, Ord)

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC f x = Count (f $ getCount x)

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 f x y = Count $ f (getCount x) (getCount y)

coerceC :: Count a -> Count b
coerceC = coerce

-- | Countable
class Countable (c :: Type) where
  count :: Count c

-- if you are using `Proxy` implement `count` from `count'` and vice versa
-- count' :: Proxy c -> Count c
-- count' = error "from count"

instance Countable Void where count = Count 0

instance Countable () where count = Count 1

instance Countable Bool where count = Count 2

instance Countable Nat where
  count :: Count Nat
  count = Count (S $ getCount $ count @Nat)

-- | Factor
class Factor (f :: Type -> Type) where
  factor :: Count c -> Count (f c)

-- factor' :: Proxy f -> Count c -> Count (f c) -- optional

instance (Factor f, Countable c) => Countable (f c) where
  count :: (Factor f, Countable c) => Count (f c)
  count = factor @f (count @c)

instance Factor Maybe where
  factor :: Count c -> Count (Maybe c)
  factor c = Count (1 + getCount c)

instance Factor Identity where
  factor :: Count c -> Count (Identity c)
  factor c = Count (getCount c)

instance Factor Proxy where
  factor :: Count c -> Count (Proxy c)
  factor = const $ (Count (S Z))

instance Factor Count where
  factor :: Count c -> Count (Count c)
  factor _ = Count inf where inf = S inf

instance Factor [] where
  factor :: Count c -> Count [c]
  factor (Count Z) = Count (S Z)
  factor _ = Count inf where inf = S inf

instance (Countable c) => Factor (Const c) where
  factor :: (Countable c) => Count c1 -> Count (Const c c1)
  factor c1 = Count (getCount (count @c))

--  Either a b = a + b
instance (Countable c) => Factor (Either c) where
  factor :: (Countable c) => Count c1 -> Count (Either c c1)
  factor c1 = Count (getCount (count @c) + getCount (c1))

-- (a,b) = a * b
instance (Countable c) => Factor ((,) c) where
  factor :: (Countable c) => Count c1 -> Count (c, c1)
  factor c = Count (getCount c * getCount (count @c))

-- a -> b = b ^ a
instance (Countable c) => Factor ((->) c) where
  factor :: (Countable c) => Count c1 -> Count (c -> c1)
  factor b = Count (getCount b ^ getCount (count @c))

instance (Factor f, Factor g) => Factor (Sum f g) where
  factor :: (Factor f, Factor g) => Count c -> Count (Sum f g c)
  factor c = Count (getCount (factor @f c) + getCount (factor @g c))

instance (Factor f, Factor g) => Factor (Product f g) where
  factor c = Count (getCount (factor @f c) * getCount (factor @g c))

instance (Factor f, Factor g) => Factor (Compose f g) where
  -- maybe?
  factor :: (Factor f, Factor g) => Count c -> Count (Compose f g c)
  factor c = Count (getCount (factor @f . factor @g $ c))

-- | Listable
class (Countable a) => Listable (a :: Type) where
  list :: [a]

-- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where list = []

instance Listable () where list = [()]

instance Listable Bool where list = [False, True]

instance Listable Nat where list = iterate (+ 1) Z

instance (Listable c) => Listable (Maybe c) where
  list :: (Listable c) => [Maybe c]
  list = Nothing : map Just (list @c)

instance (Listable c) => Listable [c] where
  list :: (Listable c) => [[c]]
  list = [] : [(l1 : l2) | l1 <- list @c, l2 <- list @[c]]

instance (Listable a, Listable b) => Listable (Either a b) where
  list :: (Listable a, Listable b) => [Either a b]
  list = map Left (list @a) <> map Right (list @b)

instance (Listable a, Listable b) => Listable (a, b) where list = [(a, b) | a <- list @a, b <- list @b]

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  list :: (Eq a, Listable a, Listable b) => [a -> b]
  list = map (\fs f -> fromJust $ lookup f fs) $ power (list @a) (list @b)
    where
      power [] bs = [[]]
      power (a : as) bs = fmap (:) (fmap ((a,)) bs) <*> power as bs

list' :: (Listable l) => Proxy l -> [l]
list' Proxy = list

count' :: (Countable l) => Proxy l -> Count l
count' Proxy = count