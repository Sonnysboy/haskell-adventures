import Control.Monad 
data Natural = Zero | Succ Natural deriving (Eq)

-- we take add to be our operation
instance Semigroup Natural where 
  (<>) = add

instance Monoid Natural where 
  mempty = Zero
  mappend = (<>)

instance Show Natural where
  show = show . go 0
    where
      go curr Zero = curr
      go curr (Succ x) = go (curr + 1) x
add :: Natural -> Natural -> Natural
add Zero x = x
add x Zero = x
add (Succ x) y = Succ (add x y)

multiply :: Natural -> Natural -> Natural
multiply Zero x = Zero
multiply x Zero = Zero
multiply x (Succ Zero) = x
multiply (Succ Zero) x = x
multiply x (Succ y) = x `add` (x `multiply` y)

--multiply x (Succ y) = x `add` (x `multiply` y)
-- multiply (Succ x) (Succ y) = Succ $ x `add` (y `add` (x `multiply` y))
-- S(x) * S(y) = S(x + (y + (x * y)))
-- S(x) * S(y)= x + S(y + (x * y))
-- S(x) * S(y) = x + y + Succ (x * y)



zero = Zero
one = Succ zero
two = Succ one


three = Succ (Succ (Succ Zero))
five = add two three



main = do
  forM_ [two `multiply` (three `add` five),(two `multiply` three) `add` (two `multiply` five)] print
  print $ multiply two three
  print $ multiply three two
  print $ multiply three two == multiply two three
