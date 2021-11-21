import Test.QuickCheck
----------------------------------------------------------------
-- Symbolic Expressions --
-- We define a new type
data Expr =
    Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    | Var String
    deriving Eq

instance Show Expr where
    show = showExpr

-- Can have variables!

ex1,ex2,ex3 :: Expr
ex1 = Mul (Add (Var "y") (Num 2)) (Var "x")
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))
ex3 = Num (-5) `Add` (Num 2 `Mul` Num 4)

vars :: Expr -> [String]
vars (Num n) = []
vars (Var s) = [s]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2

-- Cannot do _ on Add/Mul cases since the operator is a function
-- Pattern matching on functions are not allowed
-- Table contains the values of each variable
type Table = [(String, Integer)]
eval :: Table -> Expr -> Integer
eval t e         = eval' e where
    eval' (Num n)     = n
    eval' (Add e1 e2) = eval' e1 + eval' e2
    eval' (Mul e1 e2) = eval' e1 * eval' e2
    eval' (Var x)     = look x t -- or fromJust $ lookup x t 

look :: String -> Table -> Integer
look k [] = error $ "No value for " ++ k
look k ((k',v):t) | k == k'   = v
                  | otherwise = look k t

----------------------------------------------------------------
-- Lookup and the Maybe type --
-- Common to use maps
-- Data.Map has many functions that help us with this

-- Maybe is either Nothing or Just a.
-- Maybe weird, but its like this in java, something or null.

-- No good sense of exceptions, since its lazy evaluation
-- There is no sense of control flow, order of computation.

----------------------------------------------------------------
-- The Generator -- 
-- He extends the show definition to be able to show Vars
-- and the Generator for expressions by mapping Gen over a list
-- of xyz strings.

-- He also seems to insert some showing functionality here from 
-- the previous file

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Var x)     = x

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

-- Generator v2
range :: Integer
range = 4
level :: Integer
level = fromInteger range

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s, rOp s), (1, rVar)]
    where
        s' = s `div` 2
        rNum = elements $ map Num [-range..range]
        rVar = elements $ map Var ["x","y","z"]
        rOp s = do
            op <- elements [Mul, Add]
            e1 <- rExpr s'
            e2 <- rExpr s'
            return $ op e1 e2
-- rBin is either mul or add, binary operators

instance Arbitrary Expr where
    arbitrary = sized rExpr

----------------------------------------------------------------
-- Derivation using smart constructors --

derive :: String -> Expr -> Expr
derive x (Add e1 e2) = add (derive x e1) (derive x e2)
derive x (Mul e1 e2) = add (mul (derive x e1) e2)
                           (mul e1 (derive x e2))
derive x (Var y) | x == y = Num 1
derive _ _                = Num 0

add :: Expr -> Expr -> Expr
add (Num n) (Num m) = Num (n+m)
add (Num 0) e       = e
add e       (Num 0) = e
add e1      e2      = Add e1 e2

mul :: Expr -> Expr -> Expr
mul (Num n) (Num m) = Num (n*m)
mul (Num 0) e       = e
mul e       (Num 0) = e
mul (Num 1) e       = e
mul e       (Num 1) = e
mul e1      e2      = Mul e1 e2

----------------------------------------------------------------
-- Testing and Simplifying Expressions --
-- Simplification should not change the value
-- How do we test this? We can use tables (?)

-- We need to control generation of tables
-- Since table is just String Integer, we dont have control

newtype Env = Env Table
    deriving Show

instance Arbitrary Env where
    arbitrary = do
        (l,m,n) <- arbitrary 
        return $ Env [("x", l),("y", m),("z", n)]

-- Creates a table with random values mapped to x, y and z

----------------------------------------------------------------
-- Summary --
-- Recursive data types can take forms other than lists
-- Can model languages (expressions, natural languages etc)
-- Functions working with these are often recursive themselves.
-- Generating random values of recursive data types is tricky.
    -- its useful to use size parameters, restricting it.
    -- newtype is a trick to gain more control, and restrict.
