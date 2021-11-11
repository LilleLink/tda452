-- (1) Give a recursive definition of filter
-- The filter takes a predicate and applies it to a list, returning all the 
-- entries that return true when applied.

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (a:as)
    | p a       = a : filter' p as
    | otherwise = filter' p as

-- (2) (Not Higher-Order) Suppose that some phone companies bill by the 
-- second, and some others by 10-second intervals. Generalise the 
-- function callCost even more, and give a new definition for telefartCost

-- Old callCost method
callCost :: Double -> Double -> Integer -> Double
callCost openingCost perMinuteCost seconds =
    openingCost + fromInteger (seconds `div` 60) * perMinuteCost

-- New callCost
callCost' :: Double -> Double -> Integer -> Integer -> Double
callCost' openingCost intervalCost seconds intervalLength =
    openingCost + fromInteger (seconds `div` intervalLength) * intervalCost

teleFartCost seconds = callCost' 1.5 0.5 seconds 60

-- (3) Generalise the following functions by defining a higher-order 
-- function:
countEven ns     = length [() | n <- ns, even n]
countNegative ns = length [() | n <- ns, n < 0]
countFalse bs    = length [() | b <- bs, not b]

count' :: (a -> Bool) -> [a] -> Integer
count' _ [] = 0
count' p (a:as)
    | p a = 1 + count' p as
    | otherwise = count' p as

-- (4) Define (using recursion) a higher-order function iter such that
-- iter n f x applies the function f to the argument a n times. So for 
-- example iter 3 (++"!") "Hi" would give "Hi!!!".

iter :: Integer -> (a -> a) -> a -> a
iter 0 _ a = a
iter n f a = iter (n-1) f (f a)

-- (5) Define the power function from lab 1 using iter
power' :: Integer -> Integer -> Integer
power' n k = iter (k-1) (*n) n
-- Here I dont really like the k - 1 statement because of how iter is
-- defined. Dont know what would be a good solution.

-- (6) Define iter using the standard function iterate
iter' :: Integer -> (a -> a) -> a -> a
iter' n f a = iterate f a !! fromInteger n

