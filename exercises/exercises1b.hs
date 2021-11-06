import Data.List
data B = W B B B | X Int | Y B
    deriving Show

-- (1) Give a definition of a value of type B which contains at 
-- least one of each constructor

b :: B
b = Y (W (X 1) (X 3) (X 3))


-- (2) Define a (non-recursive) function isX :: B -> Bool which 
-- tests whether the given B is built with an X constructor 
-- (on the outside).

isX :: B -> Bool
isX (X i) = True
isX _ = False

-- (3) Define the typical “template” for a recursive function 
-- over type B.

-- f :: B -> ...
-- f (X i) = ...
-- f (Y b1) = ... f b1
-- f (W b1 b2 b3) = ... f b1 ... f b2 ... f b3

-- (4) Define a function containsZero :: B -> Bool which gives 
-- True only if the argument contains a zero somewhere inside 
-- (and gives False otherwise).

containsZero :: B -> Bool
containsZero (X i)
    | i == 0 = True
    | otherwise = False
containsZero (Y b) = containsZero b
containsZero (W b1 b2 b3) = containsZero b1 || containsZero b2
    || containsZero b3

-- (5) Define sumB :: B -> Int which sums all the numbers in the B.

sumB :: B -> Int
sumB (X i) = i
sumB (Y b) = sumB b
sumB (W b1 b2 b3) = sumB b1 + sumB b2 + sumB b3

-- (6) Define allInts :: B -> [Int] which gives a list of all the 
-- numbers in a given B but without duplicates (hint: you might find 
-- a useful function of type Eq a => [a] -> [a] or perhaps one of 
-- type Eq a => a -> [a] -> Bool that could help you solve this.

-- We can do this with the function "nub" which removes duplicates
-- from a list (included in ghc with the Data.List package)
allInts :: B -> [Int]
allInts (X i) = [i]
allInts (Y b) = nub (allInts b)
allInts (W b1 b2 b3) = nub (allInts b1 ++ allInts b2 ++ allInts b3)

-- (7) Define a function doubleB :: B -> B which returns the B 
-- obtained by doubling every number in the given B.

doubleB :: B -> B
doubleB (X i) = X (i * 2)
doubleB (Y b) = Y (doubleB b)
doubleB (W b1 b2 b3) = W (doubleB b1) (doubleB b2) (doubleB b3)

