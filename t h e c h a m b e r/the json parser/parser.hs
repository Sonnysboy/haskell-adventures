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

parseChar :: Parser Char
parseChar = Parser $ \x -> parseChar' x
    where parseChar' "" = Nothing
          parseChar' (x:xs) = Just (x, xs)
