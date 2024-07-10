{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wunused-matches #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return :: a -> State s a
  return a = State $ \b -> (a, b)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State g) >>= f = State $ \s -> h (g s )
    where
        h (a, s') = (runState $ f a) s'

--  data Reader s a = Reader {runReader :: s -> a }
instance Monad (Reader s) where
  return :: a -> Reader s a
  return s = Reader $ const s
  (>>=) :: Reader s a -> (a -> Reader s b) -> Reader s b
  (Reader g) >>= f = Reader $ \s -> runReader (f (g s)) s

--  data Writer w a = Writer {runWriter :: (w, a)}

-- mempty :: Monoid a => a
-- mappend :: Monoid a => a -> a -> a 
-- runWriter :: Writer w a -> (w, a)
instance Monoid w => Monad (Writer w) where
  return :: Monoid w => a -> Writer w a
  return a = Writer (mempty, a)
  (>>=) :: Monoid w => Writer w a -> (a -> Writer w b) -> Writer w b
  (Writer (s, v)) >>= f = let Writer (s', v') = f v in Writer (s <> s', v')
