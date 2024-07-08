{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module MicroLens where

import Control.Applicative
import Control.Monad (void)
import Data.Monoid
import Data.Traversable qualified as T
import Data.Tuple (swap)
import Prelude hiding (sum)

---------------------------------------------------------
-- Some basic libraries

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap :: (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap :: (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f

class (Profunctor p) => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Profunctor (->) where
  dimap f g h = g . h . f

instance Choice (->) where
  left' :: (a -> b) -> Either a c -> Either b c
  left' f = either (Left . f) Right
  right' :: (a -> b) -> Either c a -> Either c b
  right' f = either Left (Right . f)

class Contravariant f where
  contramap :: (b -> a) -> (f a -> f b)

-- Control.Applicative.Const replicated here for your
-- convenience
newtype K b a = K {getK :: b} deriving (Functor)

instance (Monoid b) => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K $ e <> f

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id {getId :: a} deriving (Functor)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

---------------------------------------------------------
-- The lens types you'll implement

-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b =
  p a (f b) -> p s (f t)

type Iso s t a b =
  forall p f.
  (Profunctor p, Functor f) =>
  Optic p f s t a b

type Lens s t a b =
  forall f.
  (Functor f) =>
  Optic (->) f s t a b

type Traversal s t a b =
  forall f.
  (Applicative f) =>
  Optic (->) f s t a b

type Fold s a =
  forall f.
  (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f.
  (Choice p, Applicative f) =>
  Optic p f s t a b

---------------------------------------------------------
---------------------------------------------------------
-- Todo

-- | A lens focusing on the first element in a pair
_1 :: Lens (a, x) (b, x) a b
_1 f x = (,snd x) <$> (f . fst $ x)

-- a lens focused on the second element of a 2-tuple
_2 :: Lens (x, a) (x, b) a b
_2 f x = (fst x,) <$> (f . snd $ x)

-- | A function which takes a lens and looks through it.
-- The type given is specialized to provide a hint as to
-- how to write 'view'. The more intuitive type for its use
-- is
--
-- @
-- view :: Lens s t a b -> (s -> a)
-- @
view :: Optic (->) (K a) s t a b -> (s -> a)
view l s = getK $ l K s

-- | A function which takes a lens and a transformation function
-- and applies that transformer at the focal point of the lens.
-- The type given is specialized to provide a hint as to how to
-- write 'over'. The more intuitive type for its use is
--
-- @
-- over :: Lens s t a b -> (a -> b) -> (s -> t)
-- @
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over l f = getId . l (Id . f)

-- | A function from a lens and a value which sets the value
-- at the focal point of the lens. The type given has been
-- specialized to provide a hint as to how to write 'set'. The
-- more intuitive type for its use is
--
-- @
-- set :: Lens s t a b -> b -> (s -> t)
-- @
set :: Optic (->) Id s t a b -> b -> (s -> t)
set l f = getId . l (Id . const f)

-- | A traversal which focuses on each element in any
-- Traversable container.

{-
}
type Traversal s t a b =
  forall f . Applicative f =>
  Optic (->) f s t a b

  a -> (f b) -> (f a) -> (f (f b))
traverse :: forall (t :: Type -> Type) (f :: Type -> Type) a b.
(Traversable t, Applicative f) =>
(a -> f b) -> t a -> f (t b) easy!
-}
elements :: (T.Traversable f) => Traversal (f a) (f b) a b
elements = traverse

-- | A function which takes a Traversal and pulls out each
-- element it focuses on in order. The type has been
-- specialized, as the others, but a more normal type might be
--
-- @
-- toListOf :: Traversal s s a a -> (s -> [a])
-- @

{-

(a -> (K (Endo [a]) a) -> s -> (K (Endo [a]) a)

f :: Optic (->) (K (Endo [a])) s s a a
f :: (a -> (K (Endo [a]) a)) -> (s -> (K (Endo [a]) s))
f :: (a -> Endo [a]) -> s -> Endo [a]
appEndo :: Endo a => a -> a -> a
getK :: K b a -> b
x :: s
-}
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf f x = appEndo (getK $ f (\a -> K $ Endo $ mappend [a]) x) []

-- | A function which takes any kind of Optic which might
-- be focused on zero subparts and returns Just the first
-- subpart or else Nothing.

-- type Optic p f s t a b =
--  p a (f b) -> p s (f t)
-- p = ->
-- f = K (First a)
-- s = s
-- t = s
-- a = a
-- b = a
-- (a -> (K (First a) a) -> s -> (K (First a) s)) -> (s -> Maybe a)
-- @
-- preview :: Traversal s s a a -> (s -> Maybe a)
-- @
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview o = getFirst . getK . o (K . First . Just)

-- | A helper function which witnesses the fact that any
-- container which is both a Functor and a Contravariant
-- must actually be empty.
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap (const ()) . void

-- Fold s a = (Contravariant f, Applicative f) => Optic (->) f s s a a
-- to :: (a -> b) -> Fold a b
-- where Fold a b = (Contravariant f, Applicative f) => Optic (->) f a a b b
-- ((a -> b) -> (b -> f b) -> a) -> (f a)

-- | A Fold which views the result of a function application
to :: (a -> b) -> Fold a b -- so we need to make an `f a`
to f bFb = coerce . bFb . f

-- | A prism which focuses on the left branch of an Either

{-
_Left :: forall a x b (p :: * -> * -> *) (f :: * -> *).
               (Choice p, Applicative f) =>
               Optic p f (Either a x) (Either b x) a b
type Optic p f s t a b = p a (f b) -> p s (f t)
so:
      p f  s            t           a b
Optic p f (Either a x) (Either b x) a b
= (Choice p, Applicative f) => p a (f (Either b x)) -> p (Either a x) (f (Either b x))
we know:
left' :: forall (p :: Type -> Type -> Type) a b c.
Choice p =>
p a b -> p (Either a c) (Either b c)

pure :: a -> f a

Either stuff:
Left :: a -> Either a b
Right :: b -> Either x b

fmap :: Functor f => f a -> f (Either a b)
so
fmap Left :: f b -> f (Either b x)
-}
_Left :: Prism (Either a x) (Either b x) a b
_Left pin = rmap convert (left' pin)
  where
    convert (Right x) = pure (Right x)
    convert (Left fb) = fmap Left fb

-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
_Right pin = rmap convert (right' pin)
  where
    convert (Left x) = pure (Left x)
    convert (Right fb) = fmap Right fb

-- | An iso which witnesses that tuples can be flipped without
-- losing any information

{-
type Iso s t a b =
  forall p f.
  (Profunctor p, Functor f) =>
  Optic p f s t a b

type Optic p f s t a b =
  p a (f b) -> p s (f t)
                                  s     t     a     b
(Profunctor p, Functor f) => Iso (a,b) (a,b) (b,a) (b,a)
-> (Profunctor p, Functor f) => Optic p f (a,b) (a,b) (b,a) (b,a)
-> (Profunctor p, Functor f) => p (b,a) (f (b,a)) -> p (a,b) (f (a,b))
rmap :: (b -> b') -> p a b -> p a b'
lmap :: (a' -> a) -> p a b -> p a' b
dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
-}
_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip f = dimap swap convert2 f
  where
    convert2 :: Functor f => f (b, a) -> f (a, b)
    convert2 f = swap <$> f