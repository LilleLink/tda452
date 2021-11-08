-- These statements redefines the default functions as P.function.
import Prelude hiding ((++),reverse, take, drop)
import qualified Prelude as P((++), reverse, take, drop)

-- Our own definition of a list using a recursive data type
data List a = Empty | Add a (List a)
-- Here "a" is out type parameter, since we want the list
-- to be able to operate on many different types.
-- Examples:
-- Add 12 (Add 3 Empty) :: List Int
-- Add "apa" (Add "bepa" Empty) :: List String

-- We can use polymorphism to operate on many types of lists
-- in out functions.
-- reverse :: [a] -> [a] where a is some type
-- We can also restrict the polymorphic type
-- sort :: Ord a => [a] -> [a]
-- This requires the type a to have an instance of Ord, in order to
-- be able to sort it.

-- Append function
-- Appends two lists together
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys -- base case, first list is empty
(x:xs) ++ ys = x : (xs ++ ys) -- put x in front, continue with xs ++ ys
-- until xs is empty

-- Reverse function
-- Reverses a list
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- This is quadratic complexity
-- This one below is linear, using an accumulator
rev :: [a] -> [a]
rev xs = revInto [] xs
    where revInto acc [] = acc
          revInto acc (x:xs) = revInto (x:acc) xs

-- Take and drop
-- Takes n elements from the list and returns them.
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs

-- Drops n elements from the list and returns the remainder
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs -- none left to drop, return remainder
drop _ [] = [] -- cant drop more, return empty
drop n (_:xs) = drop (n-1) xs -- drop n-1 from remainder

-- Props for these functions
prop_take n xs = length (take n xs) <= n
-- Will fail since n < 0.
-- We can define a condition for quickcheck using ==>
-- prop_take n xs = n >= 0 ==> length (take n xs)
-- This is ass since its quickcheck specific and some other things.
-- See notion for complete explanation.
-- There is also a catch using ==> see notion!
-- Best way to define it is:
prop_take n xs = 
	let n' = abs n in length (take n' xs) <= n'

-- let = part of expression, where = part of definition.

prop_takeDrop ::  Int -> [Int] -> Bool
prop_takeDrop n xs = drop n xs ++ take n xs == xs
-- Will be true but its not, QuickCheck doesn't know what type to use
-- so it uses the void type.

-- We need to specify like this.
-- prop_takeDrop :: [Int] -> [Int] -> Bool

