{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (a :: Nat) == (b :: Nat) :: Bool
type instance Zero == Zero = True
type instance Succ a == Succ b = a == b



type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero Zero = Zero
type instance Add Zero n = n
type instance Add (Succ n) m = Succ (Add n m)

-- to be defined

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _) = x
index (SSucc a) (VCons x xs) = index a xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate what (SSucc SZero)= VCons what VNil
replicate what (SSucc x) = VCons what (replicate what x)


-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith f VNil _ = VNil
zipWith f (VCons x VNil) (VCons y VNil) = VCons (f x y) VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ x = x
VCons a as ++ b = VCons a (as ++ b)

-- The semantics should match that of take for normal lists.
type family Take (n :: Nat) (m :: Nat) :: Nat where
  Take Zero m = Zero
  Take (Succ n') Zero = Zero
  Take (Succ n') (Succ m') = Succ (Take n' m')

take :: SNat n -> Vec s m -> Vec s (Take n m)
take SZero _ = VNil
take (SSucc n') VNil = VNil
take (SSucc n') (VCons x xs) = VCons x (take n' xs)

type family Subtract (a :: Nat) (b :: Nat) :: Nat where
  Subtract Zero b = Zero
  Subtract a Zero = a
  Subtract (Succ a) (Succ b) = Subtract a b
drop :: SNat n -> Vec s m -> Vec s (Subtract m n)
drop _ VNil = VNil
drop SZero xs = xs
drop (SSucc rest) (VCons x xs) = drop rest xs



head :: Vec s n -> s
head VNil = error "nil"
head (VCons s as) = s

tail :: Vec s (Succ n) -> Vec s n
tail (VCons s as) = as
