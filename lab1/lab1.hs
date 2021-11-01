{- Lab 1
   Date: 2021-10-30
   Authors: Axel Larsson and Simon Engström
   Lab group: TODO: fix this
 -}
--------------------------------------------
import Test.QuickCheck
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k - 1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower _ k = k + 1


-- B ------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k
   | k == 0 = 1
   | k < 0 = error "power: negative argument"
   | otherwise = product myList where myList = [n | _ <- [1..k]]

-- C ------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "power: negative argument"
   | k == 0 = 1
   | even k = power2 (n * n) (k `div` 2)
   | odd k = n * power2 n (k - 1)

-- D ------------------------
{- 
    Tests:
    Mathematical tests to make sure the function outputs expected 
    values for supported inputs:
    1. 0 to the power of any integer >0 should always be 0.
    2. 1 to the power of any positive integer should always be 1.
    3. Any integer raised to the power of 0 should always be 1.
    Program tests for making sure the functions handle unsupported types 
    or values correctly:
    4. Negative exponents should cause an error - Since its not supported
    5. Functions should only take integers - to prevent type errors
 -}

-- Tests that the power functions return the same value for some n, k. 
-- Will throw an error for k<0
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power n k == power2 n k)

-- Tests cases 1, 2, 3. Works only for n >= 0
powerTest :: Integer -> Bool
powerTest n = prop_powers n 0 && prop_powers 1 n && prop_powers 0 n

-- Tests that the power functions returns the same value for some n, k.
-- Takes absolute value of k to avoid errors.
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power n k' == power1 n k') && (power n k' == power2 n k')
   where k' = abs k