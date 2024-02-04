-- this is gonna be hard
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Char (isNumber)

newtype Parser a = Parser (String -> Maybe (a,String))


parse :: Parser a -> (String -> Maybe (a, String))
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
        -- (a, String) -> (Just (apply a function (a -> b) to type a so now it's a b so we're good and the type checker can go fuck off again!), String) 
        Just (r', s'') -> Just (r r', s'')


instance Monad Parser where
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser f) >>= g = Parser $ \x -> 
    let initial = f x
    in case initial of 
      -- (a, String)
      Just (r, s) -> parse (g r) s
      Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a -- whatever the hell this does
    Parser p <|> Parser r = Parser $ \x -> p x <|> r x
    -- the Maybe Alternative returns the first Just, i.e Just x <|> Just y = Just x



-- succeed if and only if the character parsed matches the predicate
sat :: (Char -> Bool) -> Parser Char
sat predicate = Parser parseChar'
  where parseChar' "" = Nothing
        parseChar' (x : xs) = if predicate x then Just (x, xs) else Nothing

-- parse a specfic character (as in succeeds if it parses the certain character)
char :: Char -> Parser Char
char x = sat (==x)

-- succeed for any character
next :: Parser Char
next = sat (const True)

-- Succeeds when it finds the first matching character, otherwise fails.
either :: Char -> Char -> Parser Char
either x y = char x <|> char y

between :: Char -> Char -> Parser String
between x y = char x *> many (sat $ (/=) y) <* char y



y = char '(' *> next <* (next <|> char ')')