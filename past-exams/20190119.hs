import Test.QuickCheck
import Data.List
{--
-- 1 --
fa :: String -> String
fb :: [a] -> (a,[a])
fc :: Ord a => a -> a -> a -> Bool
fd :: (a -> b) -> [[a]] -> [[b]]

map :: (a -> b) -> [a] -> [b]
map :: (b -> c) -> [b] -> [c]

-- 2 --
a. Define a function subsequence
Calculates all the subsequences of the list.
Including some, none and all elemenets of the list.
Should appear in ascending/descending order with regards to length.

subsequences "abc" = ["abc", "ab", "ac", "a", "bc", "b", "c", ""]

subsequences :: [a] -> [[a]]
subsequences []     = [[]]
subsequences (x:xs) = map (x:) rest ++ rest
    where
        rest = subsequences xs

b. isSubsequenceOf

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf ss list = ss `elem` (subsequences list)

c. prop_subsequence

prop_subsequences :: Eq a => [a] -> Bool
prop_subsequences list = all (`isSubsequenceOf` list) (subsequences list)

-- 3 --
a. 
data Equation = Eqn Expr Expr

data Expr = Num Int | Op Oper Expr Expr
    deriving Show
    
data Oper = Add | Mul | Sub
    deriving Show

b.
rExpr :: Int -> Gen Expr
rExpr s = do if s > 0 then rOp else rNum    
    where
        rOp :: Gen Expr
        rOp = do
            e1 <- rNum
            -- Only rExpr here since otherwise we cannot control the 
            -- number of expressions we generate
            e2 <- rExpr (s-1) 
            op <- elements [Add, Mul, Sub]
            return (Op op e1 e2)
        
        rNum :: Gen Expr
        rNum = do
            n <- elements [1..10]
            return $ Num n

c.
For three ints:
Add Add
Add Sub
Add Mul
Sub Sub
Sub Add
Sub Mul
Mul Mul
Mul Add
Mul Sub

n-1 operators required to produce an expression with all n numbers given
On each place there is 3 choices, Add/Sub/Mul, 3^n expressions possible.

Only handles lists of size 2 (one operator)
exprs :: [Int] -> [Expr]
exprs (x:y) = [Op op (Num x) (Num y) | op <- ops]
    where ops = [Add, Mul, Sub]

exprs :: [Int] -> [Expr]
exprs (x:xs) = [map]

-- 4 --

data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)

a.
mapTree :: (a1 -> a2) -> (b1 -> b2) -> Tree a1 b1 -> Tree a2 b2
mapTree af _  (Leaf a)       = Leaf (af a)
mapTree af bf (Node b t1 t2) = Node (bf b) (mapTree af bf t1) 
                                           (mapTree af bf t2) 

b.
Funkar men dåligt
foldTree :: Tree a (a -> a -> a) -> a
foldTree (Node func (Leaf a1) (Leaf a2))         = func a1 a2
foldTree (Node func (Leaf a) (Node nfunc t1 t2)) = func a (nfunc (foldTree t1) (foldTree t2))
foldTree (Node func (Node nfunc t1 t2) (Leaf a)) = func (nfunc (foldTree t1) (foldTree t2)) a
foldTree (Node func t1 t2)                       = func (foldTree t1) (foldTree t2)

Bättre version
foldTree :: Tree a (a -> a -> a) -> a
foldTree (Leaf a) = a
foldTree (Node f t1 t2) = f (foldTree t1) (foldTree t2)

c.

eval_v2 :: Expr -> Int
eval_v2 tree = foldTree . mapTree id evalOp . convert tree
    where
        exprToTree :: Expr -> Tree Int Oper
        exprToTree (Num x) = Leaf x
        exprToTree (Op op e1 e2) = Node op (convert e1) (convert e2)

evalOp :: Oper -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

-- 5 --
sortFiles :: [FilePath] -> IO ()
sortFiles files = do 
    contents <- sequence $ map (readFile) files
    let sortedContents = (sort . concat . map lines) contents
    sequence_ $ map putStrLn sortedContents

--}