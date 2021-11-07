module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty)
-- Every itemi of the sizeSteps array should be equal to 2.
        
sizeSteps :: [Integer]
sizeSteps = [size hand2,
    size    (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)),
    1 + size (Add (Card Jack Spades) Empty),
    2 + size Empty,
    2]

-- A1
-- Returns a string, presenting the cards of a given hand in a nice format.
display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- Returns a string representing a given card.
-- For numeric cards it shold not display Numeric {value}.
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

-- A2
-- We chose to use option 2.
-- Calculates the total value of a given hand with the given value to 
-- represent aces.
value :: Hand -> Integer
value h
    | init <= 21 = init
    | otherwise = init - (numberOfAces h * 10)
        where init = initialValue h

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h
    where
        valueRank (Numeric i) = i
        valueRank Ace = 11
        valueRank _ = 10

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h)
    | rank c == Ace = 1 + numberOfAces h
    | otherwise = numberOfAces h

-- A3
-- Determines if the player with the given hand is bust or not.
gameOver :: Hand -> Bool
gameOver h 
    | value h > 21 = True
    | otherwise = False

-- A4
-- Determines which players has won given the two hands.
-- First hand belongs to the player, second to the bank.
winner :: Hand -> Hand -> Player
winner guestHand bankHand
    | gameOver guestHand = Bank
    | gameOver bankHand = Guest
    | value guestHand > value bankHand = Guest
    | otherwise = Bank