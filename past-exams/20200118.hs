{-- 
-- Question 1 ----------------------------------------------------------------

a:
fa :: [String] -> String
fb :: [a] -> (a,a,a)
fc :: Ord a => a -> a -> [Bool] -> Bool
fd :: IO String -> IO ()

b:
-- | Merges two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) | a <= b    = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

-- Question 2 ----------------------------------------------------------------

type HP = Int                                       -- the number of "hit points"
data Card = Trainer HP | Monster Species Kind HP    deriving Show
data Species = Poopachu | Fartle | Blub             deriving (Eq,Show)
data Kind = Nature | Water | Fire                   deriving (Eq,Show)

data Hand = Last Card | Several Card Hand           deriving Show

validKinds :: Species -> [Kind]
validKinds Poopachu = [Fire]
validKinds Blub = [Water]
validKinds Fartle = [Nature,Fire,Water]

a:
-- | Checks the validity of a card
prop_Card :: Card -> Bool
prop_Card (Trainer hp)     = hp <= 5 && hp >= 1
prop_Card (Monster s k hp) = (hp <= 3 && hp >= 1) && k `elem` validKinds s

b:
-- | Recursively checks the validity of each card in the hand
prop_Hand :: Hand -> Bool
prop_Hand (Last card)         = prop_Card card
prop_Hand (Several card hand) = prop_Card card && prop_Hand hand

c:
-- | Folds the given hand by applying a function f and combining them
-- with the function op.
foldHand :: (Card -> a) -> (a -> a -> a) -> Hand -> a
foldHand f op (Several c1 (Last c2)) = op (f c1) (f c2)
foldHand f op (Several c hand)       = op (f c)  (foldHand f op hand)

d:
hitPoints :: Hand -> Int
hitPoints = foldHand cardHP (++) -- eta reduced

cardHP :: Card -> HP
cardHP (Trainer hp)     = hp
cardHP (Monster _ _ hp) = hp

e:
instance Arbitrary Card where
    arbitrary = do
        card <- elements [Trainer, Monster]
        case card of 
            Monster -> do 
                hp <- elements [1..3]
                species <- elements [Poopachu, Fartle, Blub]
                kind <- elements $ validKinds species
                return $ card species kind hp

            Trainer -> do 
                hp <- elements [1..5]
                return $ card hp 

-- Question 3 ----------------------------------------------------------------

data DTree = Q Question [(Answer,DTree)] deriving Show
type Question = String
type Answer = String

decision :: Question -> DTree
decision q = Q q []

a:
travel :: DTree
travel = Q "Is it raining" [("Yes", decision "Take the bus")
                            ("No", Q "How far is it?" [("< 1km", decision "Walk")
                                                       ("Between 1 and 10km", decision "Cycle")
                                                       ("> 10km", decision "Take the bus")
                                                       ]
                            )]

b:
allAnswers :: DTree -> [Answer]
allAsnwers ( Q _ [] )          = []
allAnswers ( Q _ ((a,dt):as) ) = a : allAnswers dt

-- Question 4 ----------------------------------------------------------------

type Sudoku = [Row]
type Row = [Maybe Int]

exampleSud :: Sudoku
exampleSud = [[blank, blank, Just 4]
             ,[blank, Just 5, blank]
             ,[Just 6, blank, blank]] 
    where blank = Nothing

-- Alternative:
type Sudoku’ = [FilledCell]
type Pos = (Int,Int)
data FilledCell = Filled Pos Int deriving (Eq,Show)

exampleSud’ :: Sudoku’
exampleSud’ = [Filled (2,0) 4, Filled (1,1) 5, Filled (0,2) 6]

a:
-- | Maps each position in the sudoku to a Maybe FilledCell depending on if
-- the cell is a Just Int or Nothing. Then each row is concatmapped with catMaybes
-- which filters out all of the Nothings, and "unwraps" Just from the values,
-- resulting in a list of FilledCells.
fromSudoku :: Sudoku -> Sudoku'
fromSudoku sud = concatMap catMaybes $ map (map posToMaybeFilledCell) positions
    where 
        positions = [(x,y) | x <- length (head sud), y <- length sud]
        posToMaybeFilledCell (x,y) = case pos !! y !! x of
            Just a  -> Just Filled pos (pos !! y !! x)
            Nothing -> Nothing
        
b:
update :: Sudoku' -> Pos -> Maybe Int -> Sudoku'

-- Removes the cell from the sudoku since the value is nothing.
update sud pos Nothing = removePos sud pos

-- Clears the cell from the sudoku if it exists, 
-- then adds the new FilledCell with the given value.
update sud pos (Just i) = Filled pos (Just i) : removePos sud pos

-- | Removes the FilledCell at the position pos if it exists.
-- Otherwise returns the same Sudoku.
removePos :: Sudoku' -> Pos -> Sudoku' 
removePos sud pos = filter (\fc -> cellToPos fc == pos) sud 

-- | Extracts the position of the given FilledCell.
cellToPos :: FilledCell -> Pos
cellToPos (Filled (x,y) _) = (x,y)

--}