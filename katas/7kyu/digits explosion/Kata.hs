module Kata (explode) where

explode :: String -> String
explode = concatMap (\x -> replicate (read [x]) x)

-- other solutions use this function:
{-
Data.Char.digitToInt
explode = concatMap (\c -> replicate (digitToInt c) c)

and this function: =<< whatever the hell that does.
i.e:
explode = (=<<) (replicate =<< digitToInt)

i have no idea

-}