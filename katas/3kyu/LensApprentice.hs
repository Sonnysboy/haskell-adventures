{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleLens where
import Control.Arrow (Arrow(first))
import Data.Tuple (swap)

-- Some functors we will need (implement them)

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f = Identity . f . runIdentity

-- |It maps every type b to a in a sense, and every function of type b -> c to the identity function on type  a
newtype Const a b = Const { getConst :: a } deriving (Eq, Show)
instance Functor (Const a) where
    fmap :: (a2 -> b) -> Const a1 a2 -> Const a1 b
    fmap f x = Const (getConst x)


-- The Lens types (given)

-- source `s`, new source `t`, focus `a`, new focus `b`
type Lens s t a b = forall f . (Functor f) => (a -> f b) -> (s -> f t)





-- Lens utility functions (implement them – follow the types!)


{-
It might help to read this type as follows:

    type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)
    "If you give me a way to put the (modified) focus into a particular functor,"
    "I will give you a way to put the (modified) source into that functor."
-}
-- extract the focus `a` from a source `s`

-- fmap lens: f0 (a -> f1 b) -> f0 (s -> f1 t)

-- fmap f (Const r) == id (Const r) == Const r
view :: Lens s t a b -> s -> a
view l s = getConst $ l Const s
-- update a focus `a` to `b` within a source `s`, yielding a new source `t`
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

-- set the focus `a` to `b` within a source `s`, yielding a new source `t`
set :: Lens s t a b -> b -> s -> t
set l b = runIdentity . l (Identity . const b)


-- Example lenses (implement them – follow the types!)

-- Tuples

-- a lens focused on the first element of a 2-tuple
_1 :: Lens (a, x) (b, x) a b
-- this is also:
{-
forall a x b f .
Functor f =>
(a -> f b) -> (a, x) -> f (b, x)
-}
_1 f x = (,snd x) <$> (f . fst $ x)

-- a lens focused on the second element of a 2-tuple
_2 :: Lens (x, a) (x, b) a b
_2 f x = (fst x,) <$> (f . snd $ x)

-- Product Types, Records, Etc.

data Person = Person { name :: String, age :: Int } deriving (Eq, Show)

-- source `s`, focus `a` (focus doesn't change type, so source doesn't change type)
type Lens' s a = Lens s s a a
-- a lens focused on the name inside a person record
-- type Lens s t a b = forall f . Functor f => (a -> f b) -> (s -> f t)
-- Lens' = Lens s s a a 
-- Lens' Person String = Lens Person Person String String
-- Lens' Person String = Functor f => (String -> f String) -> (Person -> f Person)
_name :: Lens' Person String
_name l p = flip Person (age p) <$> l (name p)


-- Something Fun (inspired by a talk by Simon Peyton Jones)

newtype TempC = TempC { getC :: Float } deriving (Eq, Show, Num)
newtype TempF = TempF { getF :: Float } deriving (Eq, Show, Num)
c_f :: TempC -> TempF
c_f (TempC c) = TempF $ (9/5 * c) + 32
f_c :: TempF -> TempC
f_c (TempF f) = TempC $ 5/9 * (f - 32)

-- the focus doesn't have to be *explicitly* in the source.
-- this lens focuses on the Celsius temp "inside" a Fahrenheit temp.
_celsius :: Lens' TempF TempC
_celsius l f = fmap c_f (l . f_c $ f)


-- Lens Composition

-- make a lens focused on the name of a person in a nested tuple
-- HINT: this is a very short one-liner. Read the description again!
_1_1_1_name :: Lens' (((Person, x), y), z) String
_1_1_1_name = _1 . _1 . _1 . _name


-- Automatic Lens Generator

-- `lens` can generate a `Lens s t a b` from a getter and setter
-- transform a soruce s to a source t
{-

    given some source of type s,
    containing a focus value of type a,
    a lens can allow the user to change the focus to type b,
    generating a new source value of type t.
-}
-- by transforming a focus a into a focus b
--  needs to output: (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- lens s transform = transform . (transform . s)
lens sToA transform afb s = transform s <$> (afb . sToA $ s)