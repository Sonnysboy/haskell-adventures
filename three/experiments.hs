data IntList = Empty | Cons Integer IntList 
  deriving (Show)

-- let's take a list:

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

-- filters

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons value rest)
  | even value = Cons value $ keepOnlyEven rest
  | otherwise = keepOnlyEven rest 
-- and the book wants me to generalize a filter so let's try that

filterIntList :: (Integer -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList predicate (Cons value rest)
 | predicate value = Cons value $ filterIntList predicate rest
 | otherwise       = filterIntList predicate rest

-- now you can do stuff like
filterOdds :: IntList -> IntList
filterOdds = filterIntList odd


-- POLYMORPHISM 
-- the generic here is `t`, t is the type variable, type variables are always lowercase
data List t = Emp | Con t (List t) -- they use E and C in the book, but im not doing that shit because they look like java generics and we're talking polymorphism and i dont wanna confuse myself

integerList :: List Integer
integerList = Con 3 $ Con 5 $ Con 6 $ Con 7 Emp -- $ being op here, as otherwise we'd write it as Con 3 (Con 5 (Con 6 (Con 7 Empty)))

boolList :: List Bool
boolList = Con True $ Con False $ Con True $ Con False Emp -- you get the idea
-- let's generalize the above filter function now:

filterList :: (t -> Bool) -> List t -> List t
filterList _ Emp = Emp 
filterList predicate (Con value rest)
 | predicate value = Con value $ filterList predicate rest
 | otherwise       = filterList predicate rest

-- One important thing to remember about polymorphic functions is that the caller gets to pick the types. When you write a polymorphic function, it must work for every possible input type. This—together with the fact that Haskell has no way to directly make make decisions based on what type something is—has some interesting implications which we’ll explore later.
-- so to write mapList we want to be able to convert say a list of strings to list of integers
mapList :: (a -> b) -> List a -> List b
mapList _ Emp = Emp 
mapList op (Con value rest) = Con (op value) $ mapList op rest



-- TOTAL and PARTIAL functions
-- a partial function is a function where given a certain input it will crash.
-- also those which might recurse infinitely.
-- functions that work for all inputs without crashing or recursing infinitely are called total functions.
-- "head" in the prelude library is an example of a partial function, it crashes for an empty list.


-- the book now shits on four of the functions in prelude:
-- head is a mistake! It should not be in the Prelude. Other partial Prelude functions you should almost never use include tail, init, last, and (!!). From this point on, using one of these functions on a homework assignment will lose style points!
-- i think i used 3 of those in week 2

-- if you have to write a partial function, you should have it return a Maybe type for the case where it fails.
-- i.e safeHead in the package `safe`:
{-
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
-}


-- if a condition is always going to be guaranteed, define the types like that and have it be reflected in how you implement it.
  -- i.e:

{-
data NonEmptyList a = NEL a [a]


nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
-}
