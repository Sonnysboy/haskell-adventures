# More polymorphism and type classes

> Haskell’s particular brand of polymorphism is known as parametric polymorphism. Essentially, this means that polymorphic functions must work uniformly for any input type. This turns out to have some interesting implications for both programmers and users of polymorphic functions.

## Parameterized Functions
Lets say we have a function
```hs
f :: a -> a -> a
```
We say that f is *parameterized* in *a*, and this style of polymorphism is called *parametric polymorphism*.

## Addressing the immediate feeling of annoyance
Parametricity doesn't restrict us, it actually just guarantees; it is much easier to use and infer about what a function's behaviour could be.

## Type classes baby oh yeah types and classes lets go time to make objects an-- WAIT WHAT
Let's look at a few functions:
```hs
(+)  :: Num a  => a -> a -> a
(==) :: Eq a   => a -> a -> Bool
(<)  :: Ord a  => a -> a -> Bool
show :: Show a => a -> String
```
Num, Eq, Ord, and Show are type classes, and we say that (==), (<), and (+) are type-class polymorphic. Type classes correspond to *sets of types*, which have certain operations defined on them, and type class polymorphic functions only work for types which are instances of the type classes in question. Let's look at eq
### hell yeah let's look at a type class it's time to create some objec-- HELP
```hs
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```
`Eq` is a type class which takes a single parameter, `a`, and any type `a` which wants to be an `instance` of `Eq` must define two functions, (==) and (/=), with the correct type signatures.

If we look at the type of (==) again:
```hs
(==) :: Eq a => a -> a -> Bool
```
`Eq a` is a *type class constraint*, saying that `a` can be any type **as long as it is an instance of Eq**

When type class methods are used, the compiler uses type inference to figure out which implementation of that method to use, based on the inferred types of its arguments (like java method overloading)

### Defining `Eq` on some type we make:
```hs

data Foo = F Int | G Char

instance Eq Foo where
    (F i1) == (F i2) = i1 == i2
    (G c1) == (G c2) = c1 == c2
    _ == _ = False
    x /= y = not (x == y)
```
We don't actually have to implement `(/=)` here, as `Eq` provides a default implementation equivalent to our own.

### The REAL Eq
Eq actually looks like this:
```hs
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```
Which means that we can either specify `(==)` OR `(/=)`, as they're in terms of each other.

Some type classes are special and can be `derived` like:
```hs
data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)
```