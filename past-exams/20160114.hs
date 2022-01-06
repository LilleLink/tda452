{-# LANGUAGE FlexibleInstances #-}
import Data.List (intersperse)
import Test.QuickCheck (Gen, Arbitrary (arbitrary), elements, sample, sized, quickCheck)
{--

-- Question 1 --

a:
xmas :: Int -> IO()
xmas n = doprint 1 where
    doprint m = if m > n then return ()
                else do
                    printCopies (n - m) " "
                    printCopies m " *"
                    putStrLn ""
                    doprint (m + 1)

    where printCopies k s = if k <= 0 then return ()
                                else do putStr s
                                    printCopies (k-1) s

xmas' :: Int -> IO ()
xmas' n = mapM_ printTreeRow [1..n]
    where 
        printTreeRow :: Int -> IO ()
        printTreeRow m = do 
                        printCopies (n - m) " " 
                        printCopies m " *"
                        putStrLn ""

        printCopies :: Int -> String -> IO ()
        printCopies k s = replicateM_ k (putStr s)

b:
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f [] = [[]] -- bad - gives an extra []
splitWhen f as = fst seg : splitWhen f (drop 1 $ snd seg)
    where seg = span (not . f) as

splitWhen' :: (a -> Bool) -> [a] -> [[a]]
splitWhen' f as = case span (not . f) as of
                (first, []) -> [first]
                (first, second) -> first : splitWhen' f (drop 1 second)

prop_splitWhen0 =
    splitWhen (== ';') "A;BB;;DDDD;" == ["A","BB","","DDDD",""]
    && splitWhen (>1) [3,0,1,2,0,0] == [[],[0,1],[0,0]]
    && splitWhen (>1) [] == [[]]

c:
-- If you split in n places, we end up with n+1 segments
prop_splitWhen :: (a -> Bool) -> [a] -> Bool
prop_splitWhen f as = length (filter f as) + 1 == length (splitWhen f as)

-- Question 2 --

fa :: Eq a => [a] -> a -> [b] -> Maybe b
fa l m n = m ‘lookup‘ zip l n

fb :: [(a -> a)] -> a -> a
fb [] a = a
fb (b:c) a = fb c (b a)

fc :: Eq a => [a] -> [[a]] -> Bool
fc (a:b) (c:d) = b /= c -- in the eq class
fc _ e = null e

fd :: a -> [[a]] -> [[a]]
fd x = map (x:)

fdExample = fd 'a' ["Hej", "på", "dig"] == ["aHej", "apå", "adig"]

-- Question 3 --
data Sudoku = Sudoku [[Int]]

ex = Sudoku
    [[3,6,0,0,7,1,2,0,0],[0,5,0,0,0,0,1,8,0],[0,0,9,2,0,4,7,0,0],
    [0,0,0,0,1,3,0,2,8],[4,0,0,5,0,2,0,0,9],[2,7,0,4,6,0,0,0,0],
    [0,0,5,3,0,8,9,0,0],[0,8,3,0,0,0,0,6,0],[0,0,7,6,9,0,0,4,3]]

a:
showSudoku :: Sudoku -> String
showSudoku (Sudoku sud) = (unlines . intersperse newLine . map showRow) sud
    where
        showRow :: [Int] -> String
        showRow cs = (concersperse "|" . map showCell) cs

        showCell :: Int -> String
        showCell c = if c == 0 then " " else show c

        concersperse x = concat . intersperse x
        f = map . map

        newLine = replicate 17 '-'

b:
block :: (Int, Int) -> Sudoku -> [[Int]]
block (c,r) (Sudoku sud) = map extractRow [r*3..(r*3) + 2]
    where
        extractRow y = [sud !! y !! x | x <- [c*3..(c*3) + 2]]

-- Question 4 --

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving Show

exTree = Node 2 (leafNode 1) (Node 1 (leafNode 1) (leafNode 0))
    where leafNode n = Node n Leaf Leaf

a:
hBalanced :: Tree a -> (Int, Bool)
hBalanced n = (height n, isBalanced n)
    where
        isBalanced :: Tree a -> Bool
        isBalanced Leaf = True
        isBalanced (Node _ t1 t2) = (isBalanced t1 && isBalanced t2) &&
                                    abs (height t1 - height t2) <= 1

        height :: Tree a -> Int
        height Leaf = 0
        height (Node _ t1 t2) = max (height t1 + 1) (height t2 + 1)

b:
allPaths :: Tree a -> [[a]]
allPaths (Node n Leaf Leaf) = [[n]]
allPaths (Leaf)             = []
allPaths (Node n t1 t2)     = map (n:) (allPaths t1 ++ allPaths t2)

c:
balTree :: Gen (Tree Bool)
balTree = sized bTree

bTree :: Int -> Gen (Tree Bool)
bTree h = do
    if h <= 0 then return Leaf else do
        value <- elements [True, False]
        diff  <- elements [(0,0), (0,1), (1,0)]
        left  <- bTree (h - 1 - fst diff)
        right <- bTree (h - 1 - snd diff)
        return (Node value left right)

instance Arbitrary (Tree Bool) where
    arbitrary = balTree

prop_balTree :: Tree Bool -> Bool
prop_balTree t = snd (hBalanced t)


--}


