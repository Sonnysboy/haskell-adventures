module TransitiveClosure where
import Data.Set

trans :: (Ord a) => Set (a, a) -> Set (a, a)
trans e = let f x = fromList [(a,d) | (a,b) <- toList x, (c,d) <- toList x, b == c]
              e2 = f e
          in if e2 `isSubsetOf` e then e else trans $ e <> e2