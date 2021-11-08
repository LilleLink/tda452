{- Lab 1
   Date: 2021-10-30
   Authors: Axel Larsson and Simon Engström
   Lab group: 12
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

    This is to make sure that 0 to the power of an integer is 0.
    1. 0 to the power of 2 should be 0. 

    This is to make sure that an integer to the power of 0 is 1.
    2. 2 raised to the power of 0 should be 1.

    This is to make sure that 1 to the power of an integer is 1.
    3. 1 to the power of 50 should be 1.

    This is to test that the function supports very large numbers
    4. 2 to the power of 100 should be 1267650600228229401496703205376.

    These are to make sure that the function works for negative bases
    5. -2 to the power of 3 should be -8.
    6. -2 to the power of 4 should be 16.

    Program tests for making sure the functions handle unsupported types 
    7. Negative exponents should cause an error - Since its not supported
    8. Functions should only take integers - to prevent type errors
 -}

-- Tests that the power functions return the same value for some n, k. 
-- Will throw an error for k<0
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power n k == power2 n k)

-- Tests well behaved test cases.
powerTest :: Bool
powerTest = prop_powers 0 2 && prop_powers 2 0 
   && prop_powers 1 50 && prop_powers 2 100
   && prop_powers (-2) 3 && prop_powers (-2) 4

-- Redifned after feedback THIS IS NOT SUBMITTED
powerTest' :: Bool 
powerTest' = and [prop_powers' n k | (n,k) <- [(0,2),(2,0),(1,50),(2,100),(-2,3),(-2,4)]]

-- Tests that the power functions returns the same value for some n, k.
-- Takes absolute value of k to avoid errors.
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n k'
   where k' = abs k

