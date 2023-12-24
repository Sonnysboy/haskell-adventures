module Main where
import Control.Arrow

yesAndNo :: (a -> Bool) -> [a] -> ([a], [a])
yesAndNo f = filter f &&& filter  (not . f)


main = do
  print $ yesAndNo even $ [1..10]