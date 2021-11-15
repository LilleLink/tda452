module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- A0
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
        (Add (Card Jack Spades) Empty)

-- Every item in the sizeSteps array should be equal to 2.
sizeSteps :: [Integer]
sizeSteps = [size hand2,
    size    (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)),
    1 + size (Add (Card Jack Spades) Empty),
    2 + size Empty,
    2]

-- A1
-- Returns a string representing the given hand in a nice format.
display :: Hand -> String
display Empty = ""
display (Add c h) = displayCard c ++ "\n" ++ display h

-- Returns a string representing a given card.
-- For numeric cards it shold not display Numeric {value}.
displayCard :: Card -> String
displayCard (Card (Numeric i) s) = show i ++ " of " ++ show s
displayCard (Card r s) = show r ++ " of " ++ show s

-- A2
-- We chose to use option 1.

-- Returns the value of a player's hand.
value :: Hand -> Integer
value h
    | init <= 21 = init
    | otherwise = init - numberOfAces h * 10
        where init = initialValue h

-- Returns the value of a given hand, using the value 11 for aces.
initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add c h) = valueRank (rank c) + initialValue h
    where
        valueRank (Numeric i) = i
        valueRank Ace = 11
        valueRank _ = 10

-- Returns the number of aces in a given hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add c h)
    | rank c == Ace = 1 + numberOfAces h
    | otherwise = numberOfAces h

-- A3
-- Determines if the player with the given hand is bust or not.
gameOver :: Hand -> Bool
gameOver h = value h > 21

-- A4
-- Determines which players has won given the two hands.
-- First hand belongs to the player, second to the bank.
winner :: Hand -> Hand -> Player
winner guestHand bankHand
    | gameOver guestHand = Bank
    | gameOver bankHand = Guest
    | value guestHand > value bankHand = Guest
    | otherwise = Bank

-- B1
-- Puts the first hand on top of the second hand.
(<+) :: Hand -> Hand -> Hand
Empty       <+ Empty       = Empty
h           <+ Empty       = h
Empty       <+ h           = h
(Add c1 h1) <+ (Add c2 h2) = Add c1 $ h1 <+ Add c2 h2

-- Property for checking if <+ is associative.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

-- Property for checking that the sizes of each hand
-- add up to the sizse of the combined hand.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 =
    size h1 + size h2 == size (h1 <+ h2)

-- B2
-- Returns a hand containing all cards in ascending order.
fullDeck :: Hand
fullDeck = full deckList Empty
    where
        full []     hand = hand
        full (c:cs) hand = full cs (Add c hand)

-- Returns a list of all cards in a normal deck.   
deckList :: [Card]
deckList = [Card r s | r <- map Numeric [2..10] ++ [Jack, Queen, King, Ace],
                       s <- [Hearts, Diamonds, Spades, Clubs]]

-- B3
-- Given a deck and a hand, draws one card from the deck to the hand.
-- Will report an error if deck is empty.
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add c h) hand = (h, Add c hand)

-- B4
-- Returns the banks played hand given a deck.
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand
    | value hand >= 16 = hand
    | otherwise = playBankHelper smallerDeck biggerHand
    where (smallerDeck,biggerHand) = draw deck hand

-- B5
-- Shuffles the given deck.
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g deck = Add card (shuffleDeck g' unshuffled)
    where (i,g') = randomR (0, size deck - 1) g
          (unshuffled,card) = drawAtIndex deck i

-- Draws a card at a index and returns the card and the rest of the hand.
drawAtIndex :: Hand -> Integer -> (Hand, Card)
drawAtIndex hand i = remove hand i Empty
    where
        remove Empty     i _       = error "removeCard: Invalid index"
        remove (Add c h) 0 removed = (removed <+ h,c)
        remove (Add c h) i removed = remove h (i-1) (removed <+ Add c Empty)

-- Property for checking that the shuffled deck contains the same cards
-- as it did before shuffling.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Helper function for the sameCards property that checks if
-- a card is in the given hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property that checks if size of deck remains the same after shuffle.
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)

-- B6
-- Code was given
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation