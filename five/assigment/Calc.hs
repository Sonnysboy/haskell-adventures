{-# LANGUAGE AllowAmbiguousTypes #-}
import ExprT;
import Parser
{-
On day one of your new job as a software engineer, you’ve been
asked to program the brains of the company’s new blockbuster prod-
uct: a calculator. But this isn’t just any calculator! Extensive focus
group analysis has revealed that what people really want out of their
calculator is something that can add and multiply integers. Anything
more just clutters the interface.
Your boss has already started by modeling the domain with the
following data type of arithmetic expressions:
-}
-- so this can either be a literal or a combination of expressions
-- i.e for (2 + 3) * 4
-- Mul (Add $ Lit 2 $ Lit 3) $ Lit 4
x :: ExprT
x = Mul (Add (Lit 2) (Lit 3)) $ Lit 4

-- this was really easy
eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2


evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Nothing -> Nothing
    Just expr -> Just $ eval expr

class Expr a where
    lit :: a -> ExprT
    add :: ExprT  -> ExprT -> ExprT
    mul :: ExprT -> ExprT -> ExprT
    -- mul :: a -> a -> a
-- add :: Expr a -> Expr a -> Expr a
    -- mul :: Expr a -> Expr-> Expr a
    -- add :: ExprT -> ExprT -> ExprT
    -- mul :: ExprT -> ExprT -> ExprT
instance Expr Integer where
    lit = Lit
    add :: ExprT -> ExprT -> ExprT
    add = Add
    mul :: ExprT -> ExprT -> ExprT
    mul = Mul

