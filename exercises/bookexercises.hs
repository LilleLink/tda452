-- Only answering the questions I feel like are worth doing
-- Far from all

thirdLetter :: String -> Char
thirdLetter s = s !! 3

rvrs :: String -> String
rvrs word = take 7 (drop 9 word) ++ take 4 (drop 5 word) ++ take 5 (word)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome as = as == reverse as

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else x*(-1)

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f t1 t2 = ((snd t1, snd t2),(fst t1, fst t2))

-- (* 9) 6 = Int
-- head [(0,"doge"),(1,"kitteh")] = (Int, String)
-- (Integer, String)
-- Bool
-- Int
-- Bool
-- Int
-- z :: Int -> Int
-- f :: Fractional a => a

-- Numeric types ------------------------------------------------------ 
-- Integral numbers : Whole numbers, positive and negative
-- Int - bounded
-- Integer - unbounded
-- CANNOT be divided by /, has to be div. 

-- Fractional numbers : Not integers, float/double/rational/scientific
-- Can be divided using /, instead of div. 

-- Num typeclass : Both Integrals and Fractionals are part of this
-- Provides definition for *, + and -, not division naturally using /.
-- Since / has the typeclass contraint Fractional a => a -> a -> a

-----------------------------------------------------------------------

-- Chapter 7 Exercises --


