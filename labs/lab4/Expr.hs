module Expr where
import Parsing
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Test.QuickCheck

-- A
-- | A datatype for representing mathematical expressions.
data Expr
    = Num Double
    | X
    | Op Binary Expr Expr
    | Func Unary Expr
    deriving (Eq)

instance Show Expr where
    show = showExpr

data Binary = Add | Mul
    deriving (Show,Eq)

data Unary = Sin | Cos
    deriving (Show,Eq)

-- | Gets the corresponding haskell function for a given binary operator.
getBinOp :: Num a => Binary -> a -> a -> a
getBinOp Add = (+)
getBinOp Mul = (*)

-- | Gets the corresponding haskell function for a given unary operator.
getUnOp :: Floating a => Unary -> a -> a
getUnOp Sin = Prelude.sin
getUnOp Cos = Prelude.cos

-- Functions for testing
x :: Expr
x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Op Add
mul = Op Mul

sin,cos :: Expr -> Expr
sin = Func Sin
cos = Func Cos

size :: Expr -> Int
size (Num _)        = 1
size X              = 1
size (Op _ e1 e2)   = size e1 + size e2
size (Func _ e)     = size e

-- B
-- | Displays expressions in a nice format,
--   including parentheses where needed.
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr X  = "x"
showExpr (Op Add e e') =
    showExpr e ++ " + " ++ showExpr e'
showExpr (Op Mul e e') =
    showFactor e ++ " * " ++ showFactor e'
    where
        showFactor (Op Add e1 e2) = "(" ++ showExpr (Op Add e1 e2) ++ ")"
        showFactor e           = showExpr e
showExpr (Func Sin e) = "sin " ++ showArg e
showExpr (Func Cos e) = "cos " ++ showArg e

-- | Helper function for showing function arguments.
showArg :: Expr -> String
showArg e@(Num n) = showExpr e
showArg X         = showExpr X
showArg e         = "(" ++ showExpr e ++ ")"

-- C
-- | Evalutes an expression given the value of x.
eval :: Expr -> Double -> Double
eval (Num n) _          = n
eval X       x          = x
eval (Op binop e1 e2) x = getBinOp binop (eval e1 x) (eval e2 x)
eval (Func unop e) x    = getUnOp unop $ eval e x

-- D
{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "(" expr ")".
-}

-- | Definitions of parsers for expressions, terms and factors.
expr, term, factor :: Parser Expr
expr   = foldl1 (Op Add) <$> chain term (char '+')
term   = foldl1 (Op Mul) <$> chain factor (char '*')
factor =  Num <$> readsP
      <|> Func Sin <$> do mapM_ char "sin"; factor
      <|> Func Cos <$> do mapM_ char "cos"; factor
      <|> char '(' *> expr <* char ')'
      <|> do char 'x'; return X

-- | Reads and parses an expression from the given string.
readExpr :: String -> Maybe Expr
readExpr str =
    case parse expr (filter (not.isSpace) str) of
        (Just (e,"")) -> Just e
        _             -> Nothing

-- | Formats the expression accordingly with respect to its associativity.
assoc :: Expr -> Expr
assoc (Op Add (Op Add e1 e2) e3) = assoc (Op Add e1 (Op Add e2 e3))
assoc (Op Mul (Op Mul e1 e2) e3) = assoc (Op Mul e1 (Op Mul e2 e3))
assoc (Op binop e1          e2)  = Op binop (assoc e1) (assoc e2)
assoc (Func unop e)              = Func unop (assoc e)
assoc e                          = e

--E
-- | Property that asserts that showing and parsing
--   does not alter the expression.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr =
    assoc expr == (assoc . fromJust . readExpr . show) expr

-- | Sized generator for expressions.
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1,rNum), (1,genX), (s,rBin), (s,rFunc)]
    where
        s' = s `div` 2

        genX = do return X

        rNum = Num <$> (arbitrary :: Gen Double)

        rBin = do
            op <- elements [Op Add, Op Mul]
            e1 <- arbExpr s'
            e2 <- arbExpr s'
            return $ op e1 e2

        rFunc = do
            func <- elements [Func Sin, Func Cos]
            e <- arbExpr s'
            return $ func e

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-- F
-- | Simplifies the given expression.
simplify :: Expr -> Expr
simplify e
    | e == simplified = e
    | otherwise = simplify simplified
    where
        simplified = simplify' e
        simplify'   (Num x)               = Num x
        simplify'   X                     = X

        simplify'   (Op Add (Num 0) e)       = simplify e
        simplify'   (Op Add e (Num 0))       = simplify e
        simplify'   (Op Mul (Num 0) _)       = Num 0
        simplify'   (Op Mul _ (Num 0))       = Num 0
        simplify'   (Op Mul (Num 1) e)       = simplify e
        simplify'   (Op Mul e (Num 1))       = simplify e

        simplify'   (Op binop (Num a) (Num b)) = Num (getBinOp binop a b)
        simplify'   (Op binop e X)           = Op binop X (simplify e)
        simplify'   (Op binop X e)           = Op binop X (simplify e)
        simplify'   (Op binop e1 e2)         = Op binop (simplify e1) 
                                                        (simplify e2)

        simplify' e@(Func _ (Num a))         = Num $ eval e 0
        simplify'   (Func unop e)            = Func unop (simplify e)

-- | Property which asserts that simplification
--   does not alter the expression.
prop_simplify :: Expr -> Double -> Bool
prop_simplify e d =
    eval e d == eval simplified d && not (containsJunk simplified)
        where
            simplified = simplify e
            containsJunk X                      = False
            containsJunk (Num _)                = False
            containsJunk (Op _ (Num _) (Num _)) = True
            containsJunk (Func _ (Num _))       = True
            containsJunk (Op Add X (Num 0))     = True
            containsJunk (Op Add (Num 0) X)     = True
            containsJunk (Op Mul X (Num 1))     = True
            containsJunk (Op Mul (Num 1) X)     = True
            containsJunk (Op Mul X (Num 0))     = True
            containsJunk (Op Mul (Num 0) X)     = True
            containsJunk (Op _ e1 e2)           = containsJunk e1 || 
                                                  containsJunk e2
            containsJunk (Func _ e)             = containsJunk e

-- G
-- | Differentiates the given expression with respect to x.
differentiate :: Expr -> Expr
differentiate = simplify . diff . simplify
    where
        diff X              = Num 1
        diff (Num _)        = Num 0
        diff (Op Add e1 e2) = Op Add (differentiate e1) (differentiate e2)
        diff (Op Mul e1 e2) = Op Add (Op Mul (differentiate e1) e2)
                                     (Op Mul e1 (differentiate e2))
        diff (Func Sin e)   = Op Mul (differentiate e) (Func Cos e)
        diff (Func Cos e)   = Op Mul (differentiate e) (Op Mul (Num (-1)) 
                                                       (Func Sin e))