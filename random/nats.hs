data Natural = Zero | Succ Natural deriving (Eq)

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
multiply (Succ x) (Succ y) = Succ $ add (add x y) (multiply x y)
-- the function proves its own associativity isnt that awesome



zero = Zero
one = Succ zero
two = Succ one


three = Succ (Succ (Succ Zero))

main = do
  print $ multiply two three
  print $ multiply three two
  print $ multiply three two == multiply two three
