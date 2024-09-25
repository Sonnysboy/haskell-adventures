# Proofs of the applicative laws for every single non-derived applicative instance in Prelude

Applicatives serve as functors with applications, and let you embed pure expresssions (with `pure`), and sequence and combine operations with `<*>` and `liftA2`.

In this I will prove all of the applicative laws for the instances in Prelude. <*> and liftA2 must follow these laws:
## <\*> and liftA2 Laws
(<\*>) = liftA2 id
liftA2 id f x y = f <$> x <*> y

Also,
The definitions must satisfy the structure laws:
## Applicative Laws
### Identity: `pure id <*> v = v`
### Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

### Homomorphism: `pure f <*> pure x = pure (f x)`
Intuitively, applying a non-effectful function to a non-effectful argument in an effectful context is the same as just applying the function to the argument and then injecting the result into the context with pure.
### Interchange: `u <*> pure y = pure (\f -> f y) <*> u`
Intuitively, this says that when evaluating the application of an effectful function to a pure argument, the order in which we evaluate the function and its argument doesn't matter.
### functor stuff: fmap g x = pure g <*> x

## Types (for me)
```hs
pure :: a -> f a
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
liftA2 :: (a -> b -> c) -> f a -> f b  -> f c
```
# Proofs
## Complex
```hs
data Complex a = !a :+ !a
instance Applicative Complex where
  pure :: a -> Complex a
  pure a = a :+ a
  (<*>) :: Complex (a -> b) -> Complex a -> Complex b
  f :+ g <*> a :+ b = f a :+ g b
  liftA2 :: (a -> b -> c) -> Complex a -> Complex b -> Complex c
  liftA2 f (x :+ y) (a :+ b) = f x a :+ f y b
```
laws: 
```
identity: pure id <*> x = x
homomorphism: pure f <*> pure x = pure (f x)
composition: (.) <*> u <*> v <*> w = u <*> (v <*> w)
interchange: u <*> pure y = pure (\f -> f y) <*> u
functor: fmap f x = pure f <*> x
```
### Identity: `pure id <*> x = x`
``` Notice the implementation of (<*>) for Complex is:
f :+ g <*> a :+ b = f a :+ g b
pure id <*> x = x
= (\a -> a) :+ (\b -> b) <*> (a :+ b) -- eta expansion
= (\a -> a) a :+ (\b -> b) b -- application
= id a :+ id b -- identity
= a :+ b -- application
= x
```
---
### Homomorphism: `pure f <*> pure x = pure (f x)`
```
Ingredients:
pure :: a -> Complex a
pure x = x :+ x
(<*>) :: Complex (a -> b) -> Complex a -> Complex b
f :+ g <*> a :+ b = f a :+ f b
```
Pf:
```
pure x = x :+ x
pure f <*> pure x 
    = (f :+ f) <*> (x :+ x) -- definition of pure
    = f x :+ f x -- definition of <*>
    = (f x) :+ (f x) -- parenthesis
    = pure (f x) -- definition of pure
```
---
### Interchange: `u <*> pure y = pure (\f -> f y) <*> u`
Ingredients:
```
pure x = x :+ x
pure (\f -> f y) = f y :+ f y
f :+ g <*> a :+ b = f a :+ g b
```
Pf:
```
u <*> pure y = u <*> (y :+ y) -- definition of pure
             = u y :+ u y -- definition of  <*>

pure (\f -> f y) <*> u
    = (\f -> f y :+ \f -> f y) <*> u -- definition of pure
    = (\f -> f y) u :+  (\f -> f y) u -- definition of <*>
    = u y :+ u y -- beta reduction
    = u <*> pure y 
```
---
### Compose: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
Ingredients:
```
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = g (f x)
pure :: a -> Complex a
pure f = f :+ f
<*> :: Complex (a -> b) -> Complex a -> Complex b
f :+ g <*> x :+ y = f x :+ g y
```
Pf:
```hs
-- we must show that pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure (.) <*> u 
    = pure (.) <*> (u1 :+ u2) -- definition of Complex
    = (.) :+ (.) <*> (u1 :+ u2) -- definition of pure
    = (.) u1 :+ (.) u2 -- definition of (<*>)
    = (\g f x -> g (f x)) u1 :+ (\g f x -> g (f x)) u2 -- definition of composition
    = (\f x -> u1 (f x)) :+ (\f x -> u2 (f x)) -- beta reduction
-- So,
pure (.) <*> u <*> v
    = pure (.) <*> (u1 :+ u2) <*> v -- see above
    = (\f x -> u1 (f x)) :+ (\f x -> u2 (f x)) <*> (v1 :+ v2) -- definition of Complex
    = (\f x -> u1 (f x)) v1 :+ (\f x -> u2 (f x)) v2 -- definition of (<*>)
    = (\x -> u1 (v1 x)) :+ (\x -> u2 (v2 x)) -- beta reduction
-- So,
pure (.) <*> u <*> v <*> w
    = pure (.) <*> (u1 :+ u2) <*> (v1 :+ v2) <*> w -- see above
    = (\x -> u1 (v1 x)) :+ (\x -> u2 (v2 x)) <*> w
    = (\x -> u1 (v1 x)) :+ (\x -> u2 (v2 x)) <*> (w1 :+ w2) -- definiton of Complex
    = (\x -> u1 (v1 x)) w1 :+ (\x -> u2 (v2 x)) w2 -- definition of (<*>)
    = u1 (v1 w1) :+ u2 (v2 w2) -- beta reduction

-- Now we must show that this is equal to u <*> (v <*> w)
-- Consider v <*> w:
v <*> w
    = v1 :+ v2 <*> w1 :+ w2 -- definition of Complex
    = v1 w1 :+ v2 w2 -- definition of <*>
-- So,
u <*> (v <*> w)
    = u1 :+ u2 <*> (v1 w1) :+ (v2 w2) -- definition of complex
    = u1 (v1 w1) :+ u2 (v2 w2) -- definition of <*>
    = pure (.) <*> u <*> v <*> w -- see above

-- QED
```
## First
`newtype First a = First { getFirst :: a }`

First's implementation looks like this:
```hs
instance Applicative First where
  pure x = First x
  a <* _ = a
  _ *> a = a
  First (a -> b) -> First a -> First b
  (<*>) = coerce
  liftA2 = coerce

```
#### Common Ingredients
Let's break down `<*>`:
```hs
(<*>) :: First (a -> b) -> First a -> First b
(<*>) = coerce
f <*> x = coerce f x
f <*> x = (coerce f) x -- (F.1)
-- therefore (coerce f) must be a First a -> First b (F.2)
```

### Identity: `pure id <*> x = x`
Pf:
```hs
pure id = First id -- definition of pure
First id = First (\x -> x) -- definition of id
First id <*> x = (coerce id) x
    -- (coerce id) must be of type First a -> First b (F.2)
                = id x
                = x -- definition of id

```



### Homomorphism: `pure f <*> pure x = pure (f x)`
Pf:
```hs
-- We assume f has type a -> b and x has type a
pure f <*> pure x = First (\x -> f x) <*> First x -- definition of pure 
First (\x -> f x) <*> First x = (coerce (\x -> f x)) x -- F.1
-- coerce (\x -> f x) :: First a -> First b -- F.2
                            -- let f' = coerce (\x -> f x) : First a -> First b
                              = f' x -- :: First b
pure (f x) = First (f x) -- First b


```



### Interchange: `u <*> pure y = (\f -> f y) <*> u`

### Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`


