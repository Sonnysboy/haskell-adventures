-- this is gonna be hard
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Char (isNumber)

newtype Parser a = Parser (String -> Maybe (a,String))


parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) = p

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap transform p = Parser $ \x ->
    let result = parse p x
    in first transform <$> result


instance Applicative Parser where
  pure a = Parser $ \x -> Just (a, x) -- questionable
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser g = Parser $ \x -> case f x of -- ok so we're parsing using f
    Nothing -> Nothing
    -- (a -> b, string)
    Just (r, s') ->  case g s' of -- Maybe (b, String)
        Nothing -> Nothing
        -- (a, String) -> (Just (apply a function (a -> b) to type a so now it's a b), String) 
        Just (r', s'') -> Just (r r', s'')





parseChar :: Parser Char
parseChar = Parser $ \x -> parseChar' x
    where parseChar' "" = Nothing
          parseChar' (x:xs) = Just (x, xs)
