import System.Random(randomRs,mkStdGen)
import Test.QuickCheck
-- The Swedish Cake problem
-- Difficulty 🌶🌶

-- A cake is left in the common room for everyone to enjoy.
-- People never take more than one piece, but obey the
-- following protocol:

-- If there is more than 150g then they take a 100g slice.
-- If there is less than 30g then they take it all.
-- If there is between 30 and 150g then they take half of the remaining cake.

-- Write a recursive function to compute the number
-- of people who will get a taste of a cake that
-- weighs x grams.
-- Try to write the type of the function before you start. 

-- Takes how much cake there is and returns how many ppl will get cek
getCakePeople :: Float -> Int
getCakePeople cakegrams
    | cakegrams < 0 = error "ANTIMATTER DETECTED"
    | cakegrams <= 30 = 1
    | cakegrams >= 150 = 1 + getCakePeople (cakegrams - 100)
    | otherwise = 1 + getCakePeople (cakegrams / 2)
-------------------------------------------------------
-- This questions is about the weekly sales from a shop,
-- where week numbers start at zero. 

-- sales i is the sales for week i.
-- Assume the week number (Int) is positive.
-- Sales is always non-negative.

sales :: Int -> Integer
sales i = randomRs (0,1000) (mkStdGen i) !! i
           -- sales in the range 0 - 1000
           -- The definition is not important!


-- Difficulty 🌶️ Give recursive definitions of functions
-- which compute:

-- (1) total sales for the first n weeks?
totalSales :: Int -> Integer
totalSales n
    | n == 0 = sales 0
    | n > 0 = sales n + totalSales (n - 1)
    | otherwise = error "Cannot get sales of negative weeks"

-- (2) highest sale in the first n weeks?
highestSaleNWeeks :: Int -> Integer
highestSaleNWeeks n
    | n == 0 = sales n
    | n > 0 && sales n > highestSaleNWeeks (n - 1) = sales n
    | otherwise = highestSaleNWeeks (n - 1)
-- This is complexity n! .. yikes
highestSaleNWeeks' :: Int -> Integer
highestSaleNWeeks' n = accumulator n (sales n)
    where
    accumulator n h
        | n == -1 = h
        | sales n > h = accumulator (n - 1) (sales n)
        | otherwise = accumulator (n - 1) h
-- This is complexity O(n) which is significantly better

-- (3) number of weeks with sales less than 100
--     in the first n weeks. 
lowestSaleWeeks :: Int -> Integer
lowestSaleWeeks n
    | n == 0 = toInteger (fromEnum (sales n < 100))
    | sales n < 100 = 1 + lowestSaleWeeks (n - 1)
    | otherwise = lowestSaleWeeks (n - 1)
-- (4) Define each of these using list comprehensions
--     instead of recursion
totalSales' :: Int -> Integer
totalSales' n = sum [sales w | w <- [1..n]]

highestSaleNWeeks'' :: Int -> Integer
highestSaleNWeeks'' n = maximum [sales w | w <- [1..n]]

lowestSaleWeeks' :: Int -> Integer
lowestSaleWeeks' n = toInteger(length [n | n <- [sales w | w <- [1..n]]
    , n < 100])

-- Using recursion, indirectly or in a helper function:
-- Difficulty 🌶🌶️
-- (4) Average sales up to and including week n?
averageSales :: Int -> Float
averageSales n = fromIntegral(accumulateTotal n) / fromIntegral n
    where
    accumulateTotal n
        | n == 0 = sales 0
        | n > 0 = sales n + accumulateTotal (n - 1)
        | otherwise = error "Cannot get sales of negative weeks"

-- Difficulty 🌶🌶🌶
-- (5) Give the week numbers of the weeks with the best
-- sales in the first n weeks.
-- Since there may be more than one week with the best sales,
-- your answer should be a list of weeks.
bestWeeks :: Int -> [Integer]
bestWeeks n
    | n == 0 && sales n > 900 = [toInteger n]
    | n == 0 && sales n < 900 = []
    | sales n > 900 = bestWeeks (n - 1) ++ [toInteger n]
    | otherwise = bestWeeks (n - 1)

-- (6) Write a quickcheck property for (5)
-- which checks that you included all the right weeks
-- in your answer, and none of the wrong ones!
prop_bestWeeks :: Int -> Bool 
prop_bestWeeks w = bestWeeks w' == [toInteger n | n <- [0..w'], sales n > 900]
    where w' = abs w








