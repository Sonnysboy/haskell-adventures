module Main where

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

shoe :: Thing
shoe = Shoe

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

failed = Failure

ok = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

data Item
  = Sword
  | Bow
  | Pickaxe
  | Axe
  deriving (Show)

data Person = Person String Int Item
  deriving (Show)

getName :: Person -> String
getName (Person name _ _) = name

monkey :: Person
monkey = Person "monkey" 43 Sword

main :: IO ()
main = do
  (print . getName) monkey
