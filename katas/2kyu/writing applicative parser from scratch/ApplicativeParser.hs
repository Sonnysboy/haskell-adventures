{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> map (\(x, y) -> (x,) (f y)) (unP p s)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) =  pmap . const
-- for my convenience
(#>) :: Parser b -> a -> Parser a
(#>) = flip (<#)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \s -> case s of
    [] -> []
    (x:xs) -> ([(xs, x) | p x])

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = predP (const True) #> x

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
-- Write an operator <@> which performs function application inside parsers: 
-- Given a parser pf which parses a function and a parser px which parses a value,
--  create a new parser which first runs pf on the input and then px on the remaining input.
-- Then the parsed function is applied to the parsed value and forms a new outcome with the remaining input.
--  Do this for all combinations of outcomes!
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P pf) <@> (P px) = P $ \s -> [(fst g, snd f (snd g)) | f <- pf s, g <- px (fst f)]

{-
Define operators <@ and @>, 
both run the left parser first and then the right on the remaining input.
Finally, the output value of the parser on which the arrow is pointing to is returned. 
This way we can consume structure, which has no further information we need,
 e.g. the symbols (, , and ) in tuples:
-}
(<@) :: Parser a -> Parser b -> Parser a
(P pa) <@ (P pb) = P $ \s -> [(fst g, snd f) | f <- pa s, g <- pb (fst f)]

(@>) :: Parser a -> Parser b -> Parser b
(P pa) @> (P pb) = P $ \s -> [(fst f, snd g) | f <- pa s, g <- pb (fst f)]

infixl 4 <@
infixl 4 @>
infixl 4 <@>


{-
Now write a combinator stringP, 
which takes a string and creates a parser which parses that string from the input.

(Hint: (:) is a function!).
-}
-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = emptyP
stringP x = parse x
    where
        parse :: [Char] -> Parser String 
        parse [] = emptyP
        parse [x] = (:) <#> charP x <@> parse ""
        parse (s:ss) = (:) <#> charP s <@> parse ss

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(P pa) <<>> (P px) = P $ \s -> pa s ++ px s  -- uhhh


infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = [snd x | x <- unP p cs, null $ fst x]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = let r = runParser p cs in if length r == 1 then Just (head r) else Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)


evalExpr :: Expr -> Int
evalExpr ZeroE = 0
evalExpr (ConstE x) = x
evalExpr (NegE e) = negate (evalExpr e)
evalExpr (BinOpE AddBO x y) = evalExpr x + evalExpr y
evalExpr (BinOpE MulBO x y) = evalExpr x * evalExpr y

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = error "parseExpr not yet implemented"