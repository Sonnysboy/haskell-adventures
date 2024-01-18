-- so what im gonna do is make a functor over a binary tree

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)


insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty 
insert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (insert x left) right
    | x >  a = Node a left (insert x right)


fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Empty . reverse

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node x l r) = flatten l ++ [x] ++ flatten r



instance Functor Tree where
    fmap f x = fromList $ map f $ flatten x



testTreeOne = [1..10]

testTreeTwo :: [Integer]
testTreeTwo = [1, 20, 3, 4, 50, 43, 2, 32]
