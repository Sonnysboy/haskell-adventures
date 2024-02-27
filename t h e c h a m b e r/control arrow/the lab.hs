-- we can think of (Arrow y) => y a b to be hom(a, b) the set of all morphisms from a to b.
module Main where
import Control.Arrow hiding (second)
import Data.Tuple

{-
Write implementations for second, (***) and (&&&). Use just (>>>), arr, and first (plus any plain functions) to implement second; after that, you can use the other combinators once you have implemented them.
-}
second :: Arrow y => y a b -> y (c, a) (c, b)
second f = arr swap >>> first f >>> arr swap

main = putStrLn "your mom"