import Test.QuickCheck

-- variable
exchangeRate :: Double
exchangeRate = 12.385

-- functions
fromSEK :: Double -> Double
fromSEK sek = sek * exchangeRate

fromGBP :: Double -> Double
fromGBP pounds = pounds / exchangeRate

-- properties (testing)
prop_exchange :: Double -> Bool -- Needed to do this to get right return value, otherwise it assumed double??
prop_exchange sek = fromGBP (fromSEK sek) ~== sek

m ~== n = abs(m - n) < epsilon -- operator!!!!
    where epsilon = 10e-14 --local definition

-- defining abs function yourself named differently from prelude abs
abs' n | n < 0 = -n
       | otherwise = n -- can also write True, but this is more readable

--if n < 0 then -n else n optional notation not using guards

---------------------------------
-- Definition by recursion
-- power n k computes n^k when kEN
-- ex: power 2 3 = 2*2*2
power :: Integer -> Integer -> Integer
power n k | k == 0 = 1 -- kan även definieras med power n 0 = 1 (pattern matching)
          | k > 0 = n * power n (k-1) -- recursion
          | otherwise = error "Power with neg exp not supported"

prop_power n k = n^k' == power n k'
    where k' = abs k -- local definition to save comp power
-- power 2 3 = 2 * power 2 2 = 2 * 2 power 2 1 = 2 * 2 * 2 * power 2 0

--------------------------------
-- Haskell is strongly typed, statically typed (checked at compiletime)
-- Bra att typdefiniera
-- Bool = True/False
-- Int = cappad integer (datorinteger), Integer = matematisk integer, stor jävel
-- Float = single precision float , Double = Double precision float
-- Char - '' ett tecken
-- String - "" flera

--------------------------------
-- Tuples are collections of things
ex1 :: Integer -> (Bool, Integer)
ex1 n = (n < 0, abs n);
-- Pattern matching to extract fst and snd of tuple
fst' (x,y) = x -- takes a pair and returns the first
snd' (x,y) = y

--------------------------------
-- Lists :O
list1 :: [String] -- brackets to typedef lists
string1 = "Spam" -- standard types to a list of characters if ":t string1" So String is just an alias as a list of chars, as seen in :i String
list1 = ["Hehe", "Cool", "Grej", string1, string1, string1]

-- :t [] returns [] :: [a], can be a list of whatever you want it to be (polymorphism?)
-- 1:[] inserts the element first in the list
-- Colon is right associative, 2:1:[] returns [2,1]
summary :: [String] -> String
summary [] = "Nothing"
summary [s] = "Just "++ s -- [s] infers a single element list with a string in it
summary [s1,s2] = s1 ++ " and " ++ s2
summary (s:rest) = s ++ " followed by a bunch of things and ending with " ++ last rest -- pattern that covers all remaining sizes of lists. last gets last of list

-- Using recursion to count elements in list
-- haskell infers len :: Num p => [a] -> p, which means that it could return any number type p, Double, Float, Int, Integer.
len :: [a] -> Int -- But we want this type specifically.
len []      = 0 -- recursion base case
len (x:xs)  = 1 + len xs

last' :: [a] -> a
last' []     = error "last of empty list" -- error case
last' [x]    = x -- base case
last' (x:xs) = last xs

-- :t returns [a] -> a, meaning any type.

-------------------------------
-- List comprehensions
-- [1..10]

-- enums
ten = [1..10]
-- list building
-- [2 * n | n <- ten] 2 times every element (kinda foreach)
-- [n | n <- ten, odd n]
-- [(n,c) | n <- [1,2,3], c <- "abc"] creates all combinations of the lists, two "loops"

-- Implementation of quicksort
qsort []     = []
qsort (n:ns) = qsort smaller ++ [n] ++ qsort bigger
    where smaller = [s | s <- ns, s <= n]
          bigger  = [b | b <- ns, b > n]

-- random testing

pythag n = [(x,y,z) | x <- [1..n],
                      y <- [x..n],
                      z <- [y..n],
                      x^2 + y^2 == z^2]

-- fibonacci with recursion, takes an Integer n, the fibonacci number to extract.
-- Returns the number
-- Uses recursion, to basically:
-- Checks if n is 0 or 1, the two base cases of fib.
-- Otherwise it calculates the fib number of n-1+n-2 which will in turn initiate recursion down to the base cases
-- Then it will get the base case values and finish the calculation where it started on otherwise.
fib :: Integer -> Integer 
fib n 
    | n < 0 = error "Neg fib not possible"
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)