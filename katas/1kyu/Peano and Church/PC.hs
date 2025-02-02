{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List
import Data.Function
import Data.Coerce

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\x y z -> ab $ x (ba y) (ba z), \x y z -> ba $ x (ab y) $ ab z)

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)


-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero :: Peano
  zero = O
  successor :: Peano -> Peano
  successor = S
  nat :: a -> (Peano -> a) -> Peano -> a
  nat a f O = a
  nat a f (S p) = f p
  iter :: a -> (a -> a) -> Peano -> a
  iter n f O = n
  iter n f (S p) = f (iter n f p)
  plus :: Peano -> Peano    -> Peano
  plus O n = n
  plus n (S m) = S (m `plus` n)
  plus (S m) n = S (m `plus` n)
  minus :: Peano -> Peano -> Peano
  minus O x = 0
  minus a O = a
  minus (S x) (S y) = minus x y
  mult :: Peano -> Peano -> Peano
  mult _ O = O
  mult m (S n) = m `plus` (m `mult` n)
  pow :: Peano -> Peano -> Peano
  pow m O = 1
  pow m (S x) = m `mult` (pow m x)
  inf :: Peano
  inf = S inf
  isoP :: ISO Peano Peano
  isoP = (id, id)
  toP :: Peano -> Peano
  toP = id

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.

instance Nat [()] where
  zero :: [()]
  zero = []
  successor :: [()] -> [()]
  successor x = x ++ x
  nat :: a -> ([()] -> a) -> [()] -> a
  nat a f [] = a
  nat a f xs = f xs
  iter :: a -> (a -> a) -> [()] -> a
  iter n f [] = n
  iter n f (_:xs) = f (iter n f xs)
  plus :: [()] -> [()] -> [()]
  plus = substR (liftISO2 isoP) plus
  minus :: [()] -> [()] -> [()]
  minus = substR (liftISO2 isoP) minus
  mult :: [()] -> [()] -> [()]
  mult = substR (liftISO2 isoP) mult
  pow :: [()] -> [()] -> [()]
  pow = substR (liftISO2 isoP) pow
-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching

newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow
  zero :: Scott
  zero = Scott const
  successor :: Scott -> Scott
  successor fun = Scott $ \_ g -> g fun
  nat :: a -> (Scott -> a) -> Scott -> a
  nat a f (Scott g) = g a f
  iter :: a -> (a -> a) -> Scott -> a
  iter n f (Scott x) = x n $ iter (f n) f

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  zero :: Church
  zero = Church (\f x -> x) -- so zero applications
  successor :: Church -> Church
  successor (Church f) = Church (\g x -> g (f g x)) -- one application 
  nat :: a -> (Church -> a) -> Church -> a
  nat a f c
    | isZero c = a
    | otherwise = f (pred c)
      where
        isZero (Church c) = c (const False) True
        pred (Church n) = Church (\f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u))
  iter :: a -> (a -> a) -> Church -> a
  iter n f (Church g) = g f n 
  plus :: Church -> Church -> Church
  plus = \(Church ahc) (Church bhc) -> Church $ \f -> (ahc f) . (bhc f)
  minus :: Church -> Church -> Church
  minus = substR (liftISO2 isoP) minus
  mult :: Church -> Church -> Church
  mult (Church a) (Church b)= Church (a . b)
  pow :: Church -> Church -> Church
  pow (Church a) (Church b)= Church (b a)
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.