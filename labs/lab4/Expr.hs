module Expr where
import Parsing
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import Test.QuickCheck

-- A
data Expr
    = Num Double
    | X
    | Add Expr Expr
    | Mul Expr Expr
    | Sin Expr
    | Cos Expr
    deriving (Eq)

instance Show Expr where
    show = showExpr

x :: Expr
x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Add
mul = Mul

sin,cos :: Expr -> Expr
sin = Sin
cos = Cos

size :: Expr -> Int
size (Num _)     = 1
size X           = 1
size (Add e1 e2) = size e1 + size e2
size (Mul e1 e2) = size e1 + size e2
size (Sin e)     = size e
size (Cos e)     = size e

-- B
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr X  = "x"
showExpr (Add e e') =
    showExpr e ++ " + " ++ showExpr e'
showExpr (Mul e e') =
    showFactor e ++ " * " ++ showFactor e'
    where
        showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
        showFactor e           = showExpr e
showExpr (Sin e) = "sin " ++ showArg e
showExpr (Cos e) = "cos " ++ showArg e

showArg :: Expr -> String
showArg e@(Num n) = showExpr e
showArg X         = showExpr X
showArg e         = "(" ++ showExpr e ++ ")"

-- C
eval :: Expr -> Double -> Double
eval (Num n) _     = n
eval X       x     = x
eval (Add e1 e2) x = eval e1 x + eval e2 x
eval (Mul e1 e2) x = eval e1 x * eval e2 x
eval (Sin e)     x = Prelude.sin $ eval e x
eval (Cos e)     x = Prelude.cos $ eval e x

-- D
{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "(" expr ")".
-}

expr, term, factor :: Parser Expr
expr   = foldl1 Add <$> chain term (char '+')
term   = foldl1 Mul <$> chain factor (char '*')
factor =  Num <$> readsP
      <|> char '(' *> expr <* char ')'
      <|> Sin <$> do mapM_ char "sin"; factor
      <|> Cos <$> do mapM_ char "cos"; factor
      <|> do char 'x'; return X

readExpr :: String -> Maybe Expr
readExpr str =
    case parse expr (filter (not.isSpace) str) of
        (Just (e,"")) -> Just e
        _             -> Nothing

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3))
assoc (Add e1          e2) = Add (assoc e1) (assoc e2)
assoc (Mul (Mul e1 e2) e3) = assoc (Mul e1 (Mul e2 e3))
assoc (Mul e1          e2) = Mul (assoc e1) (assoc e2)
assoc e                    = e

--E
prop_showReadExpr :: Expr -> Bool
prop_showReadExpr expr = 
    assoc expr == (assoc . fromJust . readExpr . show) expr

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1,rNum), (1,genX), (s,rBin), (s,rFunc)]
    where
        range = 10
        s' = s `div` 2

        genX = do return X

        rNum = Num <$> choose (-range,range)

        rBin = do 
            op <- elements [Add, Mul]
            e1 <- arbExpr s'
            e2 <- arbExpr s'
            return $ op e1 e2
        
        rFunc = do
            func <- elements [Sin, Cos]
            e <- arbExpr s'
            return $ func e

instance Arbitrary Expr where
    arbitrary = sized arbExpr

-- F
simplify :: Expr -> Expr
simplify e
    | e == simplified = e
    | otherwise = simplify simplified
        where 
        simplified = simplify' e
        simplify'   (Num x)               = Num x
        simplify'   X                     = X

        simplify'   (Add (Num 0) e)       = simplify e
        simplify'   (Add e (Num 0))       = simplify e
        simplify' e@(Add (Num a) (Num b)) = Num (a+b)
        simplify'   (Add e X)             = Add (simplify e) X
        simplify'   (Add X e)             = Add X (simplify e)
        simplify'   (Add e1 e2)           = Add (simplify e1) (simplify e2)

        simplify'   (Mul (Num 0) _)       = Num 0
        simplify'   (Mul _ (Num 0))       = Num 0
        simplify'   (Mul (Num 1) e)       = simplify e
        simplify'   (Mul e (Num 1))       = simplify e
        simplify' e@(Mul (Num a) (Num b)) = Num $ eval e 0
        simplify'   (Mul e X)             = Mul (simplify e) X
        simplify'   (Mul X e)             = Mul X (simplify e)
        simplify'   (Mul e1 e2)           = Mul (simplify e1) (simplify e2)

        simplify' e@(Sin (Num a))         = Num $ eval e 0
        simplify' e@(Cos (Num a))         = Num $ eval e 0
        simplify'   (Sin e)               = Sin (simplify e)
        simplify'   (Cos e)               = Cos (simplify e)

prop_simplify :: Expr -> Bool
prop_simplify e = eval e 42 == eval (simplify e) 42