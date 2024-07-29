This Kata will give you an introduction to applicative parsing. First we will write some basic combinators and common functions and after that, you will use them to parse our own simple kind of arithmetic expressions.

Warning: This Kata teaches you how to implement combinators for applicative functors, therefore you are not allowed to use the corresponding definitions!
What is a parser?

Parsers find structure within text, either to validate, or to get text into some representation. For example, in case we want to parse an integer from a string, we expect a value of an integer type as result of the parser. Therefore we parametrize our parser over the type of the expected value, this leads to the following definition.

newtype Parser a = P { unP :: String -> [(String, a)] }

It's just a function of the input string, returning a list of possible outcomes! A single outcome is the remaining input and the parsed value. Why multiple outcomes? Well, we could parse things in different ways. For example "1 + 2 + 3" could be parsed as (1 + 2) + 3 or 1 + (2 + 3).
Let's get to work

    Write a function pmap, which applies a function over the value result of a parser.
    Since we will use pmap often, <#> is provided as an operator equivalent.
    Write an operator <#, which takes a value and a parser, and replaces the value of all outcomes of the parser by the given value.

Now we will write some basic combinators:

    predP takes a predicate and will parse a single character, if the input contains at least one character and the predicate returns True applied to the character.
    charP takes a character and will only succeed, if the first character in the input is equal to the given character.

Sometimes we want to inject values into parsers, you will see later why this makes sense:

    Write a function inject, which returns a parser that will always succeed with the given value. The parser will not consume any input!
    Write an operator <@> which performs function application inside parsers: Given a parser pf which parses a function and a parser px which parses a value, create a new parser which first runs pf on the input and then px on the remaining input. Then the parsed function is applied to the parsed value and forms a new outcome with the remaining input. Do this for all combinations of outcomes!

(Hint: List comprehensions may come in handy!)

(inject (+) <@> inject 22) <@> inject 20 == inject 42

Did you notice the partial function application? We could also write it with pmap:

(+) <#> inject 22 <@> inject 20 == inject 42

Define operators <@ and @>, both run the left parser first and then the right on the remaining input. Finally, the output value of the parser on which the arrow is pointing to is returned. This way we can consume structure, which has no further information we need, e.g. the symbols (, , and ) in tuples:

tupleP p = (,) <#> (charP '(' @> p <@ charP ',') <@> (p <@ charP ')')

Now write a combinator stringP, which takes a string and creates a parser which parses that string from the input.

(Hint: (:) is a function!).
So many possibilities

When parsing operators of arithmetic expressions, we may want to parse either the '+' operator or the '*' operator. But how do we express that? Write an operator <<>>, which, when given 2 parsers, succeeds with all possible outcomes of both parsers!

This operator does in fact have a neutral element emptyP, which never parses anything, without even looking at the input! Therefore:

emptyP <<>> p      == p
p      <<>> emptyP == p

Now that we have a choice, write a function many, which returns a new parser, that applies the given parser zero or more times to the input. Write another function some that applies the given parser one or more times to the input!

(Hint: You don't need to fiddle with the function inside the Parser type, the written combinators suffice.)
Finally doing some work

All what we've done yet, was sticking parsers together by creating new functions, but we want to do actual work, therefore finally giving the parser function some input. Define a function runParser taking a parser and an input string, which applies the parser function to the input string. It should return the values of all outcomes, where the input has been fully consumed.

Write another function runParserUnique which does the same, but only succeeds if there was a single fully consumed input (there probably will be multiple not fully consumed inputs).
Everybody loves expressions

Now you can show what you've learned! Given an expression datatype Expr for arithmetic expressions on integers, write functions to parse and evaluate expressions. The following grammar will be used (no random whitespaces are allowed):

expr         ::= const | binOpExpr | neg | zero
const        ::= int
binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
binOp        ::= '+' | '*'
neg          ::= '-' expr
zero         ::= 'z'
int          ::= digit +
digit        ::= '0' | ... | '9'

The expression type:

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

Examples on how parseExpr should work:

parseExpr "(z + 1)"         == Just (BinOpE AddBO ZeroE (ConstE 1))
parseExpr " (z + z)"        == Nothing
parseExpr "(z+1) "          == Nothing
parseExpr "-((z * 2) + -1)" == Just (NegE (BinOpE AddBO (BinOpE MulBO ZeroE (ConstE 2)) (NegE (ConstE 1))))

Mention

You shouldn't use Control.Applicative and Data.Functor.

You should

import Prelude hiding (fmap)

