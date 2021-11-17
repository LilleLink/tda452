import Test.QuickCheck
import Test.QuickCheck (Arbitrary)
import Data.List ( nub )
import GHC.Real (underflowError)
-- Test Data Generators
---------------------------------------------
-- Type classes and overloading
-- Type classes declare a set of methods that the members
-- of the class can use!
-- There are also instances that provides implementations
-- of methods for a specific type
-- instance Num Int where -- ... like interfaces ?

-- For example: the Eq class
-- Defines equality and inequality operators
-- Defines implementations of these operators
-- Instanceof implementations ?

-- One can make a data type derive Eq without deriving Eq
-- keyword:
-- instance Eq TrafficLight where
-- Red == Red = True
-- etc
-- _ == _ = False

-- One can also define Equality for pairs if we have two types
-- of type class Eq.
-- instance Eq a, Eq b = > Eq (a,b) where
    --(x1,y1) == (x2,y2) = x1==x2 && y2==y2
-- One could also do this for lists etc.

-- There are Enum class - things that can be arranged in order
-- Allows for the ".." notation in lists
-- The bounded class applies to types with max and min values
-- like Int and Char, not Integer tho.
-- Show and Read class, show converts a -> String,
-- Read String -> a
-- One could declare the implementations of these derivations
-- one one's own datatype yourself though.

-- TEST DATA GENERATORS
-- Instructs quickCheck how to gen. values for our custom data types
-- It can do this if the type T derives the "Arbitrary" class.
-- The random generator is called "arbitrary :: Gen T"
-- For each type T that derives Arbitrary there is a random
-- value generator arbitrart :: Gen T. Gen is a monad.

---------------------------------------------
-- Do notation - not just for IO
-- Reminder about IO:

getName :: IO String
getName = do
    putStr "Enter your name > "
    getLine

doTwice :: IO a -> IO (a,a)
doTwice io = do
    a <- io
    b <- io
    return (a,b)

-- The type of doTwice getName is not (String,String)
-- It is IO (String,String), an instruction for producing a pair.

-- Haskell thinks that the type of doTwice is Monad, not just IO
-- also works for other kinds of instructions, class of instructions
-- Monads.

---------------------------------------------
-- QuickCheck Generators : arbitrary and sample
-- Performs random testing provided that the values are of the
-- type class Arbitrary. When we built things of type Gen T
-- we use do notations!

-- IO T : Instructions to build value of type T.
-- Run by ghc.

-- Gen T : Instructions to create a random value of type T.
-- Run by the QuickCheck library to perform tests.

-- sample (arbitrary :: Gen T) will sample values from the 
-- Generator of type T. This is predefined for all kinds of types T.
-- sample' generates a list of T's.
-- The type of sample is Show a => Gen a -> IO()
-- Not surprising, it needs to be able to be shown.
-- Takes a generator of a, and uses IO to get random, bc purity.

---------------------------------------------
-- Building generators with the do notation.
-- Generators are types of instructions, so we use do.

-- natural numbers
nats :: Gen Integer
nats = do
    n <- arbitrary
    return (abs n)

evens :: Gen Integer
evens = do
    n <- arbitrary
    return (2*n)

evens' :: Gen Integer
evens' = (2*) <$> arbitrary

---------------------------------------------
-- listOf, vectorOf, choose, oneof, elements, frequency

-- sample $ listOf nats produces a list of sample generated values
-- sample $ vectorOf 3 nats produces a vector of values with length n
-- sample $ choose (1,10) generates random values between 1 and 10.

-- return takes a value, and creates an instruction that delivers
-- that value. return 42 = 42, sample $ return 42 returns loads of 42

-- oneof takes a list of generators, and returns values with one of
-- those generators. Sampling it will return values generated from
-- randomly choosing a generator from the list.

-- elements, quite simple, give it a list of things to choose from
-- generates a mixture of elements from that list.

---------------------------------------------
-- Example of implementation, suit and rank
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq, Enum)
-- Making an instance
instance Arbitrary Suit where
    arbitrary = elements [Spades .. Clubs]

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Eq, Show, Ord)

-- We have numeric and royal cards, define generators for both
rNumeric, rRoyal :: Gen Rank
rNumeric = do
    n <- choose (2,10)
    return $ Numeric n -- Numeric <$> choose (2,10)
rRoyal = elements [Jack, Queen, King, Ace]

instance Arbitrary Rank where
    arbitrary = frequency [(9,rNumeric), (4,rRoyal)]

-- We can now run this since quickcheck has a generator for Rank
prop_rank :: Rank -> Bool
prop_rank (Numeric n) = n <= 10 && n > 1
prop_rank _           = True

-- classify lets us track how much numeric is used as opposed 
-- to other cards
prop_rank' :: Rank -> Property
prop_rank' r = classify (r < Jack) "Numeric" $ prop_rank r

-- We can control this percentage with frequency instead of oneof 
-- in the arbitrary instance implementation of Rank

---------------------------------------------
-- Gen Card and Gen Hand
data Card = Card Suit Rank
    deriving (Eq, Show)

instance Arbitrary Card where
    arbitrary = do
        (s,r) <- arbitrary -- Works automatically! :O reuses gens
        return $ Card s r

data Hand = Empty | Add Card Hand
    deriving Show

instance Arbitrary Hand where
    arbitrary = do
        cards <- arbitrary
        return $ toHand $ nub cards -- do toHand . nub <$> arbitrary

-- Takes elements from the [Card], combines with Add constr.
-- Base case is Empty.
toHand :: [Card] -> Hand
toHand = foldr Add Empty -- eta reducted.

toCardList :: Hand -> [Card]
toCardList Empty = []
toCardList (Add c h) = c : toCardList h

---------------------------------------------
-- Extra: What if we want to use another genetator?
-- For example, we might want to use a different generator 
-- than the arbitrary Integer one predefined by quickCheck

-- 1. Use forAll, it takes a Generator, we can choose freely.

-- 2. Make new type from the old one with its own generator.
-- newtype can only be used when the type only has 1 constructor!!
newtype Poker = Poker Hand
    deriving Show
-- Instructs the compiler that in runtime this is the same type as
-- Hand, but at compile time they are different - ish.

instance Arbitrary Poker where
    arbitrary = do
        cs <- suchThat arbitrary (\x -> length (nub x) == 5)
        return $ Poker (toHand cs)

-- prop_poker (Poker h) = ..
-- How to remove duplicates and still have 5 cards?
prop_poker :: Poker -> Bool
prop_poker (Poker h) = undefined 

size :: Hand -> Integer
size Empty = 0
size (Add c h) = 1 + size h

size' :: [a] -> Integer
size' = foldr (\ a -> (+) 1) 0