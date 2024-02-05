-- this is gonna be hard
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Map
import Json

newtype Parser a = Parser (String -> Maybe (a, String))

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
    Just (r, s') -> case g s' of -- Maybe (b, String)
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
  where
    parseChar' "" = Nothing
    parseChar' (x : xs) = if predicate x then Just (x, xs) else Nothing

-- parse a specfic character (as in succeeds if it parses the certain character)
char :: Char -> Parser Char
char x = sat (== x)

-- succeed for any character
next :: Parser Char
next = sat (const True)

full :: Parser String
full = many (sat $ const True)

seek :: Char -> Parser String
seek = many . sat . (/=)

-- Succeeds when it finds the first matching character, otherwise fails.
either :: Char -> Char -> Parser Char
either x y = char x <|> char y

-- succeeds for everything between these two characters
between :: Char -> Char -> Parser String
between x y = char x *> many (sat $ (/=) y) <* char y

digit :: Parser Integer
digit = Parser parseDigit
  where
    parseDigit [] = Nothing
    parseDigit s@(c : cs)
      | isDigit c = Just (fromIntegral $ digitToInt c, cs)
      | otherwise = Nothing

-- parse an entire number (may be negative)
num :: Parser Integer
num = maybe id (const negate) <$> optional (char '-') <*> (toInteger <$> some digit)
  where
    toInteger = Data.Foldable.foldl' ((+) . (* 10)) 0

value :: Parser JsonValue
value = JsonString <$> between '"' '"' <|> JsonInteger <$> num <|> array <|> object

string :: Parser String
string = between '"' '"'

-- so this parses a JsonObject
-- fullObject :: Parser JsonValue
-- fullObject = Parser $ \x -> case parse (eatWhitespace >> char '{' >> (parseKey)) x of
--   Just (key, rest) -> case parse (eatWhitespace >> parseInternals) rest of
--     Just (innards, rest) -> error $ (show innards) ++ " " ++ rest-- Just (Json.fromPairs innards, rest)
--     Nothing -> error $ "e: " ++ rest
--   Nothing -> error $ "x: " ++ x

object :: Parser JsonValue
object = Json.fromPairs <$> (eatWhitespace >> char '{' >> parseInternals <* eatWhitespace <* char '}')

-- chain :: Parser String -> Parser b -> Parser b
-- chain (Parser f) (Parser g) = Parser $ \x -> case f x of
--   Just (s, _) -> g s
--   Nothing -> Nothing

array :: Parser JsonValue
array =
  eatWhitespace
    >> between '[' ']'
    >>= ( \x -> Parser $ \rest -> case parse (many (value <* char ',' <|> value)) x of
            Just (value, _) -> Just (JsonArray value, rest)
            Nothing -> Nothing
        )

space = char ' '

eatSpaces = void (many (space <|> char '\t'))

eatNewlines = void (many $ char '\n')

eatWhitespace = void (many (space <|> char '\t' <|> char '\n'))

parseKey :: Parser String
parseKey =
  seek ':'
    >>= ( \x -> Parser $ \y -> case parse string x of
            Just (actualKey, _) -> Just (actualKey, y)
            Nothing -> Nothing
        )

keyed :: Parser (String, JsonValue)
keyed = Parser $ \x ->
  let key = parseKey -- ok so now we have this key
   in case parse key x of
        Just (key, rest) ->
          --          get rid of : and try to find a value from it
          case parse (char ':' >> eatWhitespace >> value) rest of
            Just (value, rest') -> Just ((key, value), rest')
            Nothing -> Nothing -- we'dprobably wanna throw an error for this
        Nothing -> Nothing

-- this can parse the guts of an object but kind of scares me
parseInternals :: Parser [(String, JsonValue)]
parseInternals = eatWhitespace >> many (optional (char ',') >> eatWhitespace >> keyed <* char ',' <|> keyed)

-- so we can use >> for getting things OUT of strings like
-- (char ':' >> full) ":monkey" = monkey.
-- what >> does is it just consumes whatever we give it and passes the rest of the string into the second function

-- y = char '(' *> next <* (next <|> char ')')
-- testing one: {\"string one\":\"value one\",\"number one\":1}
exampleInternals = "\"ppu\": 55, \"id\": \"0001\",  \"type\": \"donut\",  \"name\": \"Cake\""

complexNested = "{\"id\": \"0001\",\"type\": \"donut\",\"name\": \"Cake\",\"ppu\": 55,\"batters\":{\"batter\":[{ \"id\": \"1001\", \"type\": \"Regular\" },{ \"id\": \"1002\", \"type\": \"Chocolate\" },{ \"id\": \"1003\", \"type\": \"Blueberry\" },{ \"id\": \"1004\", \"type\": \"Devil's Food\" }]},\"topping\":[{ \"id\": \"5001\", \"type\": \"None\" },{ \"id\": \"5002\", \"type\": \"Glazed\" },{ \"id\": \"5005\", \"type\": \"Sugar\" },{ \"id\": \"5007\", \"type\": \"Powdered Sugar\" },{ \"id\": \"5006\", \"type\": \"Chocolate with Sprinkles\" },{ \"id\": \"5003\", \"type\": \"Chocolate\" },{ \"id\": \"5004\", \"type\": \"Maple\" }]}"

-- simpleNested ="{\"batters\":{\"batter\":[{ \"id\": \"1001\", \"type\": \"Regular\" },{ \"id\": \"1002\", \"type\": \"Chocolate\" },{ \"id\": \"1003\", \"type\": \"Blueberry\" },{ \"id\": \"1004\", \"type\": \"Devil's Food\" }]},\"topping\":[{ \"id\": \"5001\", \"type\": \"None\" },{ \"id\": \"5002\", \"type\": \"Glazed\" },{ \"id\": \"5005\", \"type\": \"Sugar\" },{ \"id\": \"5007\", \"type\": \"Powdered Sugar\" },{ \"id\": \"5006\", \"type\": \"Chocolate with Sprinkles\" },{ \"id\": \"5003\", \"type\": \"Chocolate\" },{ \"id\": \"5004\", \"type\": \"Maple\" }]}"
-- arrays break things
simpleNested = "\"outside\":\"the object\",\"monkey1\" : { \"id\" : 123, \"name\": \"monkey1\", \"nested\" : {\"nested object key\": \"nested object value\"}}, \"outside\":\"the object\", \"another\": {\"object\" : \"its a string\"}"

withAnArray = "\"outside\":\"the object\",\"array\":[1,2,3,4],\"monkey1\" : { \"id\" : 123, \"name\": \"monkey1\", \"nested\" : {\"nested object key\": \"nested object value\"}}, \"outside\":\"the object\", \"another\": {\"object\" : \"its a string\"}"
withAnArrayBraced = "{\"outside\":\"the object\",\"array\":[1,2,3,4],\"monkey1\" : { \"id\" : 123, \"name\": \"monkey1\", \"nested\" : {\"nested object key\": \"nested object value\"}}, \"outside\":\"the object\", \"another\": {\"object\" : \"its a string\"}}"

parseJson :: String -> Maybe JsonValue
parseJson x = fst <$> parse object x

thingy = "\"object\" : [{ \"inside\" : \"array\"}]"


(Just x) = parse parseInternals withAnArray