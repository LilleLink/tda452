import Test.QuickCheck ( Arbitrary, elements, arbitrary, sample, Gen )
-- Question 1 --
trueProps :: [a -> Bool] -> a -> [a -> Bool]
trueProps ps a = [prop | prop <- ps, prop a]

trueProps' :: [a -> Bool] -> a -> [a -> Bool]
trueProps' [] a     = []
trueProps' (p:ps) a | p a = p : trueProps' ps a
                    | otherwise = trueProps' ps a

trueProps'' :: [a -> Bool] -> a -> [a -> Bool]
trueProps'' ps a = filter (\p -> p a) ps

-- Question 2 --
data Color = Red | Green | Blue | Yellow
    deriving (Show, Eq)

data Attr = Skip | Reverse | Draw2
    deriving (Show, Eq)

data WildAttr = Choose | Draw4
    deriving Show

data Uno = Regular Color Int | Special Color Attr | Wild WildAttr
    deriving Show

-- Question 3 --
validCard :: Uno -> Bool
validCard (Regular c r)    | r >= 0 && r <= 9 = True
                           | otherwise        = False
validCard (Special c attr) = True
validCard (Wild wattr)     = True

-- Question 4 -- 
fullDeck :: [Uno]
fullDeck = [Regular c r | c <- colors, r <- [0..9]] ++
           [Special c attr | c <- colors, attr <- [Skip, Reverse, Draw2]] ++
           concat (replicate 4 [Wild wattr | wattr <- [Choose, Draw4]])
    where 
        colors = [Red, Green, Blue, Yellow]

-- Question 5 --
-- | Checks if the first card (first argument) can be put on top of the other
-- according to uno rules.
validPlay :: Uno -> Uno -> Bool
-- Any card on top of a wild card, and reverse
validPlay _ (Wild _) = True 
validPlay (Wild _) _ = True

-- Any colored card with the same color
validPlay (Regular c1 _) (Special c2 _) | c1 == c2  = True 
                                        | otherwise = False
validPlay (Special c1 _) (Regular c2 _) | c1 == c2  = True
                                        | otherwise = False

-- Any numeric card                                        
validPlay (Regular c1 r1) (Regular c2 r2)   | c1 == c2 || r1 == r2 = True
                                            | otherwise = False
validPlay (Special c1 a1) (Special c2 a2)   | c1 == c2 || a1 == a2 = True
                                            | otherwise = False                           

-- Question 6 --
validPile :: [Uno] -> Bool 
validPile pile = and $ zipWith validPlay pile (drop 1 pile)

-- Question 7 --
instance Arbitrary Uno where
    arbitrary = do elements fullDeck