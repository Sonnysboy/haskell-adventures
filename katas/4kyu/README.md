In this kata we will implement five of the most fundamental monads.

Newcomers to the functional world find monads to be one of the most intimidating concepts but on a basic level - they are not too difficult to understand.

A datatype forms a monad if it is possible to complete the following definitions such that the monad laws (described below) hold. There's nothing more to it! For a more intuitive understanding then there are a plethora of tutorials which use (sometimes wild) analogies to explain this concept.

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

Monad laws

return x >>= f = f x
m >>= return = m
(m >>= f) >>= g = m >>= (\x -> f x >>= g)

It turns out that many different types of computation can be encapsulated by monads. For example the Maybe monad encapsulates a computation which can fail and State a computation with mutable state.

The five we will implement here are Identity, Maybe, State, Writer and Reader.

Hint: https://www.haskell.org/haskellwiki/Monad_tutorials_timeline

Note: Please feel free to contribute!
