-- so what im gonna do is make a functor over a tree

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



-- basically this allows you to map over the tree
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

tree = fromList [1..10]

f x y = ((>>) <$> print <*> pure) (x+y)
main = do
    -- holy and it preserves the invariant too
    print $ fmap (+40) tree


