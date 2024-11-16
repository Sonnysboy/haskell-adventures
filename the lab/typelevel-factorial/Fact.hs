{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Kind
data Nat = Z | S Nat deriving Show

type family (a :: Nat) :+: (b :: Nat) :: Nat where
    Z :+: Z = Z
    Z :+: b = b
    (S  a) :+: b = S (a :+: b)

type family (a :: Nat) :*: (b :: Nat) :: Nat where
    a :*: Z = Z
    Z :*: a = Z
    S Z :*: b = b
    a :*: (S b) = a :+: (a :*: b) :: Nat

type family Fact (a :: Nat) where
    Fact (S Z) = S Z
    Fact (S a) = S a :*: Fact a
