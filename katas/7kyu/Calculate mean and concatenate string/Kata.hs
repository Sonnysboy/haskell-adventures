module Kata (mean) where
import Text.Read
import Data.Maybe

mean :: [Char] -> (Double, String)
mean lst = let
              nums = map (read . return) (Prelude.filter (\x -> isJust (readMaybe (return x) :: Maybe Int)) lst)

           in (sum nums / fromIntegral (length nums), filter (\x -> isNothing (readMaybe (return x) :: Maybe Int)) lst)

-- better solutions used `partition`