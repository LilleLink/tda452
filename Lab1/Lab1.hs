{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
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
stepsPower _ k = k - 1


-- B ------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k 
   | k == 0 = 1
   | k < 0 = error "Power with negative exponents not supported"
   | otherwise = product myList where myList = [n | _ <- [1..k]]

-- Why non exhaustive?
-- Could do take and repeat instead of replicate, but it corrects to replicate

--prop_power1 :: Integer -> Int -> Bool
--prop_power1 n k = n^k' == power1 n k'
   --where k' = abs k

-- C ------------------------
-- power2

power2 :: Integer -> Integer -> Integer 
power2 n k
   | k < 0 = error "Not defined for negative exponents"
   | k == 0 = 1
   | even k = power2 (n * n) (k `div` 2)
   | odd k = n * power2 n (k - 1)

-- D ------------------------
{- 

<Describe your test cases here>

 -}

-- 
prop_powers = undefined

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined
