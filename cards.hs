
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving Show 

data Colour = Red | Black
    deriving Show

data Rank = Numeric Int | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

-- Get colour of a given suit
colour :: Suit -> Colour
colour Spades = Black -- Alternatively you can use "case analysis"
colour Clubs = Black
colour _ = Red

rankBeats :: Rank -> Rank -> Bool 
rankBeats r1 r2 = r1 > r2