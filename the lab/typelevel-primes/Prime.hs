{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

data Nat = Z | S Nat deriving Show

type family (a :: Nat) == (b :: Nat) :: Bool where
  Z == Z = True
  Z == b = False
  b == Z = False
  S a == S b = a == b

type family (a :: Nat) :< (b :: Nat) :: Bool where
  m :< Z = False
  Z :< S n = True
  (S m) :< (S n) = m :< n

type family Choice a b (c :: Bool) where
    Choice a _ False = a
    Choice _ b True = b

type family (a :: Nat) :%: (b :: Nat) :: Nat where
    Z :%: _ = Z
    a :%: (S Z) = Z
    a :%: b = Choice ((a :-: b) :%: b) a (a :< b)

type family (a :: Nat) :-: (b :: Nat) :: Nat where
    Z :-: a = Z
    a :-: Z = a
    S a :-: S b = a :-: b

type family Prime (a :: Nat) :: Bool where
    Prime (S Z) = True
    Prime (S (S Z)) = True
    Prime (S x) = PrimeDecider (S x) x

type family PrimeDecider (n :: Nat) (i :: Nat) :: Bool where
    PrimeDecider n Z = True
    PrimeDecider n (S Z) = True
    PrimeDecider n i = Choice (PrimeDecider n (i :-: S Z)) False (n :%: i == Z)
