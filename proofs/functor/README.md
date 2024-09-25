So I am going to prove that the functor laws hold for every single functor defined in `base`.

I will be skipping the ones that derive the instance

# Functor Laws
```hs
fmap id = id
fmap (f . g) = fmap f . fmap g
```
But from the free theorem from fmap, we only need to prove `fmap id = id`
