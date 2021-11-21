import Test.QuickCheck
----------------------------------------------------------------
-- Recursive data types --
-- Most are tree shaped

-- Example:
data Expr
    = Num Integer 
    | Add Expr Expr
    | Mul Expr Expr
    deriving Eq
-- More than 1 recursive example => tree shaped

instance Show Expr where
    show =  showExpr


ex1 :: Expr
ex1 = Mul (Add (Num 1) (Num 2)) (Num 4)
ex2 :: Expr
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

-- | Function to evaluate expressions
eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

----------------------------------------------------------------
-- Showing arithmetic expressions --
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

----------------------------------------------------------------
-- Making an Manual Instance of Show --
-- See the data definition
-- Makes Show point to showExpr

----------------------------------------------------------------
-- QuickCheck Generator for Recursice Data - sized --

-- | Random Expr
range :: Integer
range = 4
level = fromInteger range

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s, rBin s)]
    where
        rNum = elements $ map Num [-range..range]
        rBin s = do
            let s' = s `div` 2
            op <- elements [Mul, Add]
            e1 <- rExpr s'
            e2 <- rExpr s'
            return $ op e1 e2
-- rBin is either mul or add, binary operators

instance Arbitrary Expr where 
    arbitrary = sized rExpr
-- Sized takes a generator of type Int -> (Gen a) like rExpr
-- and returns a generator which uses its own random number generator
-- which grows from small to large.

----------------------------------------------------------------
-- Completing the Arithmetic Game
main :: IO ()
main = do
    es <- sample' $ rExpr level -- Cant mix these two, two types of monads e <- rExpr level
    let e = es !! level
    putStrLn $ "What is the value of " ++ show e
    ans <- getLine 
    let v = show $ eval e
    if ans == v
        then putStrLn "Correct!"
        else putStrLn $ "Fail! Correct answer was : " ++ show v
    main

-- read reads readable things and turns them into as
-- we dont want to crash though, read will crash if it cant read the input
-- we use show eval e instead to compare strings
-- ans :: String == show $ eval e :: String

-- We use sample' above since it takes a Gen a and returns a IO [a]
-- We cant mix Gen and IO, so this is what we have to do.

----------------------------------------------------------------
-- Symbolic Expressions -- 
-- See next file
