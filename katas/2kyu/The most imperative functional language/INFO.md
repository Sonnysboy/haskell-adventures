A monster's story

Frank N. Stein likes to cobble together different paradigms and languages. This time, he thought about the factorial, and quickly wrote down

factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result

Pleased with his solution, he orders you to take it and use it in your program. The caveat? He want's the program in Haskell. If you don't manage it he'll use your brain to create a new warrior, so lets get to work.
Task

Your task is to write def, var, lit, while and the arithmetical operators. Note that the operators should take both a variable (created with var) or a literal as right hand side:

def $ do
  a <- var 1
  b <- var 2
  a += b
  a += lit 1
  return a 
-- should be 4

Note that the initial solution won't contain type signatures. It's up to you to determine or create the needed types. All values will be Integer.

Your operators will be tested on three functions.
Remarks

Although this kata shows that it's possible to write Haskell in an imperative way, don't start doing this everywhere. Mutability is a nice feature sometimes, but GHC can optimize your usual non-mutable Haskell code pretty well. Also, always benchmark first, optimize later.

The title is inspired by this stackoverflow question

-- todo: fill description with text, so that the hints don't show up on 1920x1080
--
--
--
--
--
--
--
--

Hints
Some type hints

data SomeMonad   a
data SomeVariable

def   :: SomeMonad (SomeVariable a) -> Integer
var   :: a -> SomeMonad (SomeVariable a)
lit   :: a -> (SomeVariable a)
while :: (SomeVariable a) -> (a -> Bool) -> SomeMonad () -> SomeMonad ()

(+=), (-=), (*=) :: (SomeVariable a) -> ??? -> SomeMonad ()

Feel free to use any other approach or types, as long as the functions are semantical correct.
