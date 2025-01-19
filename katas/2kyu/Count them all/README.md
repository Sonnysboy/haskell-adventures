Every type a :: * can be mapped to Natural number of its inhabitants (possibly zero or infinite). In this kata we'll develop this mapping with Count for some common types.

data Nat = Z | S Nat deriving (Show, Eq, Ord)
newtype Count (a :: *) = Count { getCount :: Nat } deriving (Show, Eq, Ord)

There are some useful functions for working with Count (you are free to delete/rename/ignore them, they won't be tested):

mapC :: (Nat -> Nat) -> Count a -> Count b
liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
coerceC :: Count a -> Count b

To create "function"  from a type to Nat we'll use typeclass:

class Countable c where
  count :: Count c
  -- count' :: Proxy c -> Count c -- optional, you may use TypeApplication

You are free to use

    Proxy: data Proxy (a :: k) = Proxy. Then implement count' from count and vice versa (same with factor and list). Now you could use count', factor', list' in instances:

    count' (Proxy :: Proxy Bool) `shouldBe` Count 2

    Type Application: add pragma {-# LANGUAGE TypeApplications #-} to the top and ignore/remove count', factor', list'. Usage:

    count @Bool `shouldBe` Count 2

Your first task is to provide Countable instances:

instance Countable Void
instance Countable ()
instance Countable Bool
instance Countable Nat


As well as type of kind * could be mapped to Nat, type of kind * -> * could be mapped to Nat -> Nat, we'll do it with additional typeclass:

class Factor (f :: * -> *) where
  factor :: Count c -> Count (f c)
  -- factor' :: Proxy f -> Count c -> Count (f c)

instance (Factor f, Countable c) => Countable (f c) where
  count = error "of course it is not useless"

Your second task is to provide following instances of Factor:

instance Factor Maybe
instance Factor Identity
instance Factor Proxy
instance Factor Count
instance Factor []
instance Countable c => Factor (Const c)
instance Countable c => Factor (Either c)
instance Countable c => Factor ((,) c)
instance Countable c => Factor ((->) c)
instance (Factor f, Factor g) => Factor (Sum f g)
instance (Factor f, Factor g) => Factor (Product f g)
instance (Factor f, Factor g) => Factor (Compose f g)

NOTE:

newtype Identity a = Identity {runIdentity :: a}
newtype Const a b = Const {getConst :: a}
data Sum f g a = InL (f a) | InR (g a)
data Product f g a = Pair (f a) (g a)
newtype Compose f g a = Compose {getCompose :: f (g a)}


Your last task is to prove that you understand what on earth are you counting by providing following instances of Listable:

class Countable a => Listable a where
  list :: [a] -- list of all inhabitants
  -- list' :: Proxy a -> [a]
-- | genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void
instance Listable ()
instance Listable Bool
instance Listable Nat

instance Listable c => Listable (Maybe c)
instance Listable c => Listable [c]
instance (Listable a, Listable b) => Listable (Either a b)
instance (Listable a, Listable b) => Listable (a, b)
instance (Eq a, Listable a, Listable b) => Listable (a -> b)

Order of inhabitants in infinite lists won't be tested.

P. S. If you liked counting then you could count some natural transformations in Yoneda Lemma.

P. P. S. Preloaded module for local testing:

module Counting.Preloaded where
import Control.Arrow ((***))

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
  a * b = iterNat (b+) a Z
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
  fromEnum x = iterNat (+1) x 0

instance Real Nat where toRational = toRational . fromEnum

instance Integral Nat where
  quotRem a Z = error "divide by zero"
  quotRem a b = until ((< b) . snd) (S *** subtract b) (Z, a)
  divMod = quotRem
  toInteger n = iterNat (+1) n 0

