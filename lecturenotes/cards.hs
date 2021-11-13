
import Test.QuickCheck

-------------Suit-------------
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)

------------Colour--------------
data Colour = Red | Black
    deriving (Show, Eq)

-- Get colour of a given suit
colour :: Suit -> Colour
colour Spades = Black -- Alternatively you can use "case analysis"
colour Clubs = Black
colour _ = Red

-------------Rank-------------
data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

prop_rankBeats :: Rank -> Rank -> Bool
prop_rankBeats a b = rankBeats a b || rankBeats b a

-------------Card-------------
data Card = Card Rank Suit
    deriving Show

-- Inspector functions
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

colourCard :: Card -> Colour
colourCard c = colour (suit c)

cardBeats :: Card -> Card -> Bool
cardBeats c d
    | suit c == suit d = rank c `rankBeats` rank d
    | otherwise        = False

------------Hand-------------
data Hand = Empty | Add Card Hand -- Our own data type
    deriving Show
-- Two choices of constructors 
-- Empty, and Adding a card to a hand
-- I.e empty and non empty

ex1 = Add david Empty
    where david = Card King Hearts
ex2 = Add death ex1
    where death = Card Ace Spades

size :: Hand -> Int
size Empty = 0
size (Add c h) = 1 + size h

-- Kinda like len of list [a]

-- handBeats: A hand beats a card
handBeats :: Hand -> Card -> Bool 
hand Empty c = False
handBeats (Add c' h) c = c' `cardBeats` c || h `handBeats` c
-- "The card beats the hand if it beats the current card
-- and the rest of the hand"
-- c' is the card we are on, h is the remainder of the hand
-- c is the card to beat

-- Three ways to define a recursive split function over hand
splitHand :: Hand -> (Hand, Hand)

-- 1. Simple double traversal
splitHand h = (select Red h, select Black h)

-- helperfunction
select :: Colour -> Hand -> Hand
select col Empty = Empty
select col (Add c h)
    | col == colourCard c = Add c (select col h)
    | otherwise = select col h

-- This function sends the hand to a helper function that 
-- separates the hands given a color to look for.

-- If the color of card cmatches the one give, it adds it to the
-- return-value hand, that is defined recursively as the hand
-- containing the current mathing card and the rest of the cards
-- in the hand that also match.
-- If the card does not match it simply defines the return value
-- hand recursively by just calling itself again with the now 
-- smaller hand, not adding the current card. 

-- 2. Direct recursive definition
splitHand' :: Hand -> (Hand, Hand)
splitHand' Empty     = (Empty, Empty)
splitHand' (Add c h) 
    | colourCard c == Red = (Add c reds, blacks)
    | otherwise           = (reds, Add c blacks)
    where (reds, blacks) = splitHand' h

-- When the hand we get is empty, return empty tuple
-- When its not empty, we check wether its red or black
-- and add the card depending on the guards
-- we define (reds, blacks) as the result of calling
-- the splitHand function again with h, including all but the
-- latest card we were on, traversing recursively down to an 
-- empty hand, with the result residing in the (reds, blacks)
-- tuple defined att the bottom with the local definition.

-- 3. Single recursion using a helper function
-- (using accumulating parameters)
splitHand'' :: Hand -> (Hand, Hand)
splitHand'' h = split h Empty Empty
    where split Empty     reds blacks = (reds, blacks)
          split (Add c h) reds blacks
              | colourCard c == Red = split h (Add c reds) blacks
              | otherwise           = split h reds (Add c blacks)


-- When the h parameter to split is empty, every card will be
-- in "Empty Empty", we have split the cards.

-- When its not empty we want to check whether the current card
-- c the color red, and recall the function with the c added to reds
-- and the blacks as-is. If it is a black card we call split again,
-- but the other way around, adding c to the blacks parameter.

