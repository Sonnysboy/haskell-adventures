module Codewars.StringMaker where

makeString :: String -> String
makeString a = foldr ((:) . head) "" (words a) -- point-free!