{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe {runMaybe :: forall b. b -> (a -> b) -> b}

newtype SList a = SList {runList :: forall b. b -> (a -> SList a -> b) -> b}

newtype SEither a b = SEither {runEither :: forall c. (a -> c) -> (b -> c) -> c}

newtype SPair a b = SPair {runPair :: forall c. (a -> b -> c) -> c}

toPair :: SPair a b -> (a, b)
toPair (SPair maker) = maker (,)

fromPair :: (a, b) -> SPair a b
fromPair (x, y) = SPair (\p -> p x y)

fst :: SPair a b -> a
fst (SPair x) = x (\a b -> a)

snd :: SPair a b -> b
snd (SPair x) = x (\a b -> b)

swap :: SPair a b -> SPair b a
swap (SPair x)= x (\a b -> fromPair (b,a))

curry :: (SPair a b -> c) -> (a -> b -> c)
curry user x y = user (fromPair (x, y))

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f x = let (a, b) = toPair x in f a b

toMaybe :: SMaybe a -> Maybe a
--
-- SMaybe (\x _ -> x) = Nothing
-- SMaybe (\_ f -> f x) = Just x
-- so if we pass in Nothing Just, we get our answer
toMaybe (SMaybe maker) = maker Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Prelude.Just x) = SMaybe (\_ f -> f x)
fromMaybe _ = SMaybe Prelude.const

-- so if the SMaybe takes the form (\x _ -> x) it's nothing,
-- if it takes the form (\_ f -> f x) it's a Just with value `x`
-- SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
isJust :: SMaybe a -> Bool
isJust (SMaybe maybe) = maybe False (const True)

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = map (\(SMaybe sm) -> sm (error "Nothing") id) . ScottEncoding.filter isJust

--  SEither ~ forall c. (a -> c) -> (b -> c) -> c
--  SEither itself is pretty much just a comonad isnt it
-- SEither f g --> f is the mapping to apply to a "Left", g is the one to apply to a "Right"
toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither (\f _ -> f a)
fromEither (Right a) = SEither (\_ f -> f a)

isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (const True) (const False)

isRight :: SEither a b -> Bool
isRight (SEither f) = f (const False) (const True)

-- okay so i'll assume that this returns two lists for the lefts and the rights
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition s = fromPair (map (\(SEither l) -> l id (error "not left")) $ ScottEncoding.filter isLeft s, map (\(SEither l) -> l (error "not right") id) $ ScottEncoding.filter isRight s)

filter :: (k -> Bool) -> SList k -> SList k
filter p (SList ls) = ls (SList const) (\head rest -> if p head then cons head $ ScottEncoding.filter p rest else ScottEncoding.filter p rest)

{-
-- SList a ~ forall b. b -> (a -> SList a -> b) -> b
examples
toList (SList const) `shouldBe` ([] :: [Int])
toList (SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))) `shouldBe` [1,2]
-}
-- toList (SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))) `shouldBe` [1,2]
-- let's think about this:
-- call this:  l = SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))
-- and call (fl = runSList l) = \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))
--  and fl _ f = f 1 (SList $ \_ g -> g 2 (SList $ \end _ -> end)) { definition of const }
toList :: SList a -> [a]
toList (SList l) = l [] (\prev cons -> (:) prev $ toList cons)

fromList :: [a] -> SList a
fromList [] = SList (\prev cons -> prev)
fromList (x : xs) = SList $ (\prev cons -> cons x (fromList xs))

cons :: a -> SList a -> SList a
cons a l = SList $ (\_ cons' -> cons' a l)

concat :: SList a -> SList a -> SList a
concat (SList xs) ys = xs ys (\head rest -> cons head $ concat rest ys)

-- empty list l = SList (\k _ -> k)
-- SList a ~ forall b. b -> (a -> SList a -> b) -> b
-- okay now im a little confused but it works so
null :: SList a -> Bool
null (SList l) = l True (const (const False))

length :: SList a -> Int
length (SList l) = l 0 (\prev cons -> 1 + length cons)

-- λ runList (fromList [1,2,3,4]) (error "") (const) = 1 so we have a way to get into things.
-- so this is actually kind of like pattern matching:
-- for some application: (SList l) (\empty -> ...) (\head rest -> ...)
map :: (a -> b) -> SList a -> SList b
map f (SList l) = l (SList const) (\head rest -> cons (f head) (map f rest))

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList as) (SList bs) = as (SList const) (\head rest -> bs (SList const) (\bhead brest -> fromPair (head, bhead) `cons` zip rest brest))

foldl :: (b -> a -> b) -> b -> SList a -> b
-- z + (l1 + (l2 + (l3 + (...))))
foldl f z (SList ls) = ls z (\head rest -> foldl f (f z head) rest)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr k z (SList ls) = ls z (\head rest -> k head $ foldr k z rest)

take :: Int -> SList a -> SList a
take 0 _ = SList const
take n (SList l) = l (SList const) (\head rest -> cons head (take (n - 1) rest))
