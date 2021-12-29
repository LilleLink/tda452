import Test.QuickCheck ( Arbitrary, elements, arbitrary, sample, Gen, frequency, sized)
import Control.Monad (replicateM)
import Data.List (nub)
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

-- PART 2 --
type NamedMaze t = (t, Maze t)

data Maze t = Exit | Choice [NamedMaze t]
  deriving Show

-- For example

woods :: NamedMaze String
woods = ("Woods", Choice [("To Town"     , Exit),
                          ("To the Cliff", Choice [("Heaven", Exit),
                                                   ("Hell", deadEnd)] )
                         ]
        )

deadEnd :: Maze t
deadEnd = Choice []

-- Question 8 --
allExits :: NamedMaze t -> [[t]]
allExits (s, Exit)      = [[s]]
allExits (s, Choice ps) = map (s:) $ concatMap allExits ps

-- Question 9 --
data Map = MapExit | DeadEnd | Move (Direction -> Map)

data Direction = LeftFork | Straight | RightFork
  deriving (Eq, Enum, Show)

-- for example

woods' :: Map
woods' = Move (\d -> case d of
        LeftFork -> MapExit
        Straight -> Move (\d -> case d of
                    LeftFork -> MapExit
                    Straight -> DeadEnd
                    RightFork -> DeadEnd)
        RightFork -> DeadEnd)

mapToMaze :: Map -> Maze Direction
mapToMaze (Move mf) = Choice [(LeftFork, mapToMaze (mf LeftFork)),
                              (Straight, mapToMaze (mf Straight)),
                              (RightFork, mapToMaze (mf RightFork))]
mapToMaze DeadEnd = deadEnd
mapToMaze MapExit = Exit

-- Question 10 --
instance (Arbitrary t, Eq t) => Arbitrary (Maze t) where
    arbitrary = sized rMaze

mazePlaces :: Maze t -> [t]
mazePlaces Exit = []
mazePlaces (Choice places) = map fst places


rMaze :: (Arbitrary t, Eq t) => Int -> Gen (Maze t)
rMaze s = frequency [(s,rChoice s), (1,rExit)]
    where
        rExit = return Exit
        rChoice :: (Arbitrary t, Eq t) => Int -> Gen (Maze t)
        rChoice s = do
            nc <- elements [0..4]

            ts <- do replicateM nc arbitrary
            let nubbedts = nub ts 

            mazes <- do replicateM (length nubbedts) (rMaze (s `div` 2))

            let cs = zip ts mazes
            return (Choice cs)