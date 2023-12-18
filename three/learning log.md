# 03: Recursion patterns, polymorphism, and the Prelude

Honestly, there wasn't much in this one, but I learned a lot on the side from other things.

## Total and Partial functions

A total function is one that will never crash, no matter what input you give it. A partial function is one that might crash. To quote the book,

> head is a mistake! It should not be in the Prelude. Other partial Prelude functions you should almost never use include tail, init, last, and (!!)

In other words... well im not sure. what the haskell

## Recursion Patterns

Basically just extreme abstraction, you can see in `expertiments.hs`:

```hs
exampleList :: IntList
exampleList = Cons (-1) (Cons 2 (Cons 4 (Cons 6 Empty)))

-- we can write it like this :
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons value rest) = Cons (abs value) (absAll rest)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons value rest) = Cons (value * value) $ squareAll rest

--

-- but say we define a function like this
mapIntList :: (Integer -> Integer) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList op (Cons value rest) = Cons (op value) $ mapIntList op rest

-- now we can write these functions like this

addOne :: Integer -> Integer
addOne x = x + 1

square :: Integer -> Integer
square x = x * x

addOneList = mapIntList addOne

squareList = mapIntList square
```

## Generics? Polymorphism.

It seems like haskell developers love making everything as generic as physically possible, which I respect.

So we can turn our IntList:

```hs
data IntList = Empty | Cons Integer IntList
  deriving (Show)
```

Into a generic list as such:

```hs
data List t = E | C t (List t)
```

Where `t` is the data type, and we can create a `List` like this:

```hs
list :: List Integer
list = C 4 $ C 5 $ C 6 $ C 7 E
listBool :: List Bool
listBool = C True $ C False E
```

## Let's talk about $

Not the money, this is a function in haskell.

```
ghci> :t ($)
($) :: (a -> b) -> a -> b
```

This is basically a replacement for parenthesis (i think?). You can see it in use above with `C 4 $ C 5 $ C 6 $ C 7 E` as a replacement for `C 4 (C 5 (C 6 (C 7 E)))`, which is more suited for lisp programmers.

Based on the ones that I've talked to, haskell devs like to use as little parenths as possible, and so this dollar sing function is brilliant and makes things look nicer.

## Tuples

Say we have a `x = (4, 3)`.
Then `fst x = 4`, and `snd x = 3`. That's fun.
We can pattern match them like in `downOne`:

```hs
downOne :: [(Integer, Int)] -> [(Integer, Int)]
downOne tuples = filter (\(x1, x2) -> 0 < x2) $ map (\(x1, x2) -> (x1, x2 - 1)) tuples
```

Where x1, x2 are matched to the left and right values of the tuple

## Lambdas

Yeah, `\` is definitely some sort of lambda operator, and we can use it like this:
`\a b c d -> <stuff>` i.e in use of `sortBy` (again in `Golf.hs`)

```hs
sortBy (\(x1, _) (x2, _) -> compare x1 x2) (map (head &&& length) $ group $ sort list)
```

## New builtins:

- `unlines` joins a list with `\n`
- `filter` filters things
- `map` maps things
- `length`
- `sort`
- `&&&` no idea what the hell this thing does
- `x elem y` is the same as $x \in y$

## Commentary

This assignment was really annoying but it was fun. And by this assignment, I mean the histogram part. I'd never actually run across this problem before, and it took me almost 2 hours to do. It's actually a code golf, but I'm not going to golf it any further. I asked a few friends who are haskell-fluent to take a crack at golfing these functions and I'll update the repo once they get back to me.

That's all for part 3, see you in part 4.
