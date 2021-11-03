import System.Random(randomRs,mkStdGen)

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


-- Difficulty 🌶️Give recursive definitions of functions
-- which compute:

-- (1) total sales for the first n weeks?
-- (2) highest sale in the first n weeks?
-- (3) number of weeks with sales less than 100
--     in the first n weeks. 
-- (4) Define each of these using list comprehensions
--     instead of recursion

-- Using recursion, indirectly or in a helper function:
-- Difficulty 🌶🌶️
-- (4) Average sales up to and including week n?

-- Difficulty 🌶🌶🌶
-- (5) Give the week numbers of the weeks with the best
-- sales in the first n weeks.
-- Since there may be more than one week with the best sales,
-- your answer should be a list of weeks.

-- (6) Write a quickcheck property for (5)
-- which checks that you included all the right weeks
-- in your answer, and none of the wrong ones!









