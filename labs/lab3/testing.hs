module Sudoku where

import Test.QuickCheck
import Data.Maybe ( isNothing, fromJust, listToMaybe, catMaybes, mapMaybe )
import Data.Char
import Data.List ( nub, transpose )

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (length rows == 9) &&
    all (\x -> length x == 9) rows &&
    all (all checkCell) rows
    where
        checkCell (Just n) = 1 <= n && n <= 9
        checkCell Nothing = True

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = not (any (any isNothing) rows)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = do
    mapM_ putStrLn (toString sud)
    where
        toString :: Sudoku -> [[Char]]
        toString (Sudoku rows) = map (map toChar) rows

        toChar :: Cell -> Char
        toChar (Just n) = intToDigit n
        toChar Nothing = '.'


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
    s <- readFile file
    return $ Sudoku $ map (map toCell) (lines s)
    where
        toCell :: Char -> Cell
        toCell '.' = Nothing
        toCell c
                | '1' <= c && c <= '9' = Just $ digitToInt c
                | otherwise = error "Invalid characters in file."


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell =
    frequency [(9, nothingGen), (1, rJustInt)]
    where
        nothingGen :: Gen Cell
        nothingGen = do return Nothing
        rJustInt :: Gen Cell
        rJustInt = do
            int <- chooseInt(1,9)
            return $ Just int


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
        Sudoku <$> vectorOf 9 (vectorOf 9 cell)

-- * C3
-- | Property function for checking if a given soduko is valid.
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- | Checks that the given block doesn't contain any duplicate cells.
isOkayBlock :: Block -> Bool
isOkayBlock block = length ints == (length . nub) ints
    where
        ints = filter (/= Nothing) block


-- * D2

-- | Returns a list of each block in the sudoku. All 9 rows, 9 columns,
-- and 9 3x3 blocks using the helper function squareBlock.
blocks :: Sudoku -> [Block]
blocks sud = [squareBlock sud (x*3) (y*3) | x <- [0..2], y <- [0..2]] ++
    rows sud ++
    transpose (rows sud)

-- | Extracts a 3x3 block from a given sudoku at the given coordinates
-- x and y.
squareBlock :: Sudoku -> Int -> Int -> [Cell]
squareBlock sud x y = [rows sud !! r !! c | r <- [y..y+2], c <- [x..x+2]]

-- | Checks that there are 9 rows, 9 columns and 9 3x3 blocks of length 9.
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length bs == 27 && all (\x -> length x == 9) bs
    where bs = blocks sud

-- * D3
-- | Checks that a given sudoku does not contain duplicate integers in any 
-- column, row, or 3x3 block.
isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

-- | Returns all the blank cells in the sudoku as a list of coordinates.
blanks :: Sudoku -> [Pos]
blanks sud = [(x,y) | x <- [0..8], y <- [0..8], cellIsEmpty (x,y) sud]

-- | Checks if a given position in a sudoku is empty. 
cellIsEmpty :: Pos -> Sudoku -> Bool
cellIsEmpty (x,y) (Sudoku rows) | isNothing (rows !! x !! y) = True
                                | otherwise = False

-- | Property function that checks that the allBlankSudoku is all blanks.
prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks sud = 81 == length (blanks allBlankSudoku)


-- * E2

-- | Updates the value at a given index in a list and returns the updated 
-- list
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) | i >= length xs && i < 0
                = error "(!!=) Array index out of bounds"
             | null xs = error "(!!=) List empty"
             | otherwise = take i xs ++ [y] ++ drop (i+1) xs

-- Cases that can go wrong:
-- i is out of bounds (too large or negative)
-- The list is empty
-- These will throw errors

-- | Checks that an updated row is in fact updated at the given index and
-- that it has the same length.
prop_bangBangEquals_correct :: [Int] -> (Int, Int) -> Bool
prop_bangBangEquals_correct xs (i, x) = bxs !! i == x &&
                                        length bxs == length xs
                                            where bxs = xs !!= (i,x)


-- * E3

-- | Replaces the cell at the given position with a new cell in the sudoku.
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (x,y) c = Sudoku (take x r ++ updatedRow ++ drop (x+1) r)
    where
        r = rows sud
        updatedRow = [(r !! x) !!= (y,c)]

-- | Checks that the sudoku got updated at the correct position
prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated sud (x,y) c = rows updated !! x !! y == c
    where updated = update sud (x,y) c


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sud = listToMaybe sol
    where sol = solve' sud (blanks sud)

solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' sud bs | not (isSudoku sud && isOkay sud) = []
              | isFilled sud                     = [sud]
              | otherwise = mapMaybe solve alternatives--catMaybes [solve $ update sud b x | x <- map Just [1..9]]
                where
                    alternatives = [update sud b x | x <- map Just [1..9]] 
                    b = head bs

-- * F2


-- * F3


-- * F4