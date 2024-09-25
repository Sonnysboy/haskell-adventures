{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OddsAndEvens where

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) where
  -- | Zero is even.
  ZeroEven :: Even Z
  -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) where
  -- | One is odd.
  OneOdd :: Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd = NextEven ZeroEven
oddPlusOne (NextOdd z) = NextEven (oddPlusOne z)

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family Add (n :: Nat) (m :: Nat) :: Nat

type instance Add Z Z = Z

type instance Add Z m = m

type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd m = oddPlusOne m
oddPlusOdd (NextOdd n) m = NextEven (oddPlusOdd n m)

-- | Proves even + odd = odd
-- 0 + e = e
-- even + odd = odd
-- 2 + 1 = 3
-- 4 + 5 = 9
-- Succ e + z = Succ (e + z)
-- so this is equal to NextOdd (e + z)
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven e = e
evenPlusOdd (NextEven e) z = NextOdd (evenPlusOdd e z)

-- | Proves odd + even = odd
-- 1 + 2 = 3
-- 1 + 4 = 5
-- 1 + 6 = 7
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven OneOdd ZeroEven = OneOdd
oddPlusEven OneOdd (NextEven x) = NextOdd (oddPlusEven OneOdd x)
oddPlusEven (NextOdd y) x = NextOdd (oddPlusEven y x)

-- | Multiplies two natural numbers.
type family Mult (n :: Nat) (m :: Nat) :: Nat

type instance Mult _ Z = Z

type instance Mult n (S z) = Add n (Mult n z)

-- | Proves even * even = even
--
-- 2 * 2 = 4
-- 2 * 4 = 8
-- nextEven (2 * 2) = 6
-- nextEven (nextEven (2 * 2)) = 8
-- 2 * 4 = 2 * 2 * 2 = 2^3
-- 2 * 6 = 2 * 2 * 2 * 2 = 2^4
-- 2 * 8 = 2 * 2 * 2 * 2 * 2 = 2^5
-- = (2 * (2 * (2 * (2 * 2))) = (2 * 2) * (2 * 2) * (2) = (Add 2 (2 * 1)) * (Add 2 (2 * 1)) * Add 2 (2 * 0)
-- -> (n * (NextEven z)) = n * (2 + z)
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven (NextEven z) n = evenPlusEven n (evenTimesEven z n)-- also covers 0 * 0
evenTimesEven _ ZeroEven = ZeroEven
--   Expected: Even (Add n1 (S (S (Add n1 (Mult (S (S n1)) n2)))))

-- | Proves odd * odd = odd
-- 1 * 3 = 3
-- 1 * 5 = 5
-- 1 * 7 = 7
-- 3 * 3 = 9
-- 3 * 5 = 15 == 3 + 3 + 3 + 3 + 3 = (2 + 1) + (2 + 1) + (2 + 1) + (2 + 1) + (2 + 1) = (1 + 1 + 1) + (1 + 1 + 1) ... etc
-- 3 * 7 = 21
-- 3 * 9 = 27
-- 5 * 3 = 5 + 5 + 5 = (2 + (2 + 1)) + (2 + (2 + 1)) + (2 + (2 + 1))
-- 7 * 3 = 7 + 7 + 7 = (2 + (2 + (2 + 1))) + (2 + (2 + (2 + 1))) + (2 + (2 + (2 + 1)))
-- 7 * 5 = ...
-- n * NextOdd z
-- (NextOdd )
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd OneOdd = OneOdd
oddTimesOdd n (NextOdd z) = oddTimesOdd n (evenPlusOdd (NextEven ZeroEven) z)

-- | Proves even * odd = even

{-
2 * 1 = 2 + 0
2 * 3 = 2 + 2 + 2

We're going to need to reduce these down to ones and then add up from there


-}
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven OneOdd = ZeroEven
evenTimesOdd n (NextOdd z) = evenTimesOdd n (evenPlusOne (oddPlusOdd OneOdd z))

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven _ ZeroEven = ZeroEven
oddTimesEven z (NextEven n) = oddTimesEven z (oddPlusOne (evenPlusOne n))
