import GHC.Unicode (isSpace)
import Data.Char (isSpace)
import Data.List (sort, group, groupBy)
-- Applies a funtion f to all elements of list xs
map' :: (t -> a) -> [t] -> [a]
map' f xs = [f x | x <- xs]

-- Filters all x from xs ifthe property p x holds.
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- These to follow a general pattern
-- It operates on the case and the rest of the list with some operator (func).
sum' [] = 0
sum' (n:ns) = n + sum' ns

and' [] = True 
and' (b:bs) = b && and' bs

-- These can be generalised with foldr:
foldr' op b [] = b
foldr' op b (x:xs) = x `op` foldr' op b xs

-- Now this can be called with foldr' (+) 1 [2,3] to sum
-- foldl also exists, it is left associative instead.

-- Lines/unlines
-- Lines converts a string with newline chars to an array with lines.
-- Unlines does the reverse.

-- We define these ourselves using fold

unlines' ss = foldr joinNl "" ss
    where joinNl s1 s2 = s1 ++ "\n" ++ s2
-- In this example, joinNl is the operator sent to the foldr function,
-- defined in the local definition.

-- takeLine/takeWord

takeLine "" = ""
takeLine (c:cs) | c /= '\n' = c:takeLine cs
                | otherwise = ""

takeWord "" = ""
takeWord (c:cs) | not (isSpace c) = c:takeWord cs
                | otherwise = ""


-- Copy paste -- sign to do a generalization through a higher order function.
-- How do we generalize this?

takeWhile' :: (a -> Bool) -> [a] -> [a] 
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                    | otherwise = []
-- where p is the predicate we take.

-- So how do we define takeLine using this?
-- Either using a where like before, or LAMBDA (anonymous function)
takeLine'' cs = takeWhile' (\x -> x /= '\n') cs

-- Here the lambda expression starts at the backslash,
-- then it takes the parameter x, the body starts after "->"

-- We could also do it without lambda, with sections
-- map (+1) [1,2,3] = [2,3,4]
-- filter (<0) [1,-2,3] = [-2]
-- takeWhile (0<) [1,-2,3] = [1]

-- so (a op) b = a op b
-- and (op a) b = b op a

-- Like this
takeLine'' cs = takeWhile' (/= '\n') cs

-- dropWhile, partner function to take
lines' [] = []
lines' css = takeWhile (/= '\n') css 
    : lines' (drop 1 (dropWhile (/= '\n') css))
-- Takes characters until it failes the property.
-- Then it adds the rest of the lines recursively, by:
-- dropWhile statement - removes the first statement, already added
-- drops 1, this will be the newline character, we dont want it.
-- Then calling lines again with the new modified [Char]

-- We can generalize this!
-- The rise for higher order functions arise when we have repeated patterns of 
-- computation!

nosh = "spam,eggs,chips,brat"
commsep css = takeWhile (/= ',') css 
    : commsep (drop 1 (dropWhile (/= ',') css))

-- COPYING - higher order function needed:
segments _ [] = []
segments p xs = takeWhile p xs : segments p (drop 1 $ dropWhile p xs)

-- Partial application
-- Another way to build functions.

-- meaningless function to show
f :: Char -> Bool -> String -> String
f c b s = c:s ++ show (not b) ++ s

-- To call this we write f 'a' True "hello"
-- BUT this is shortened, we apply the function one argument at a time
-- ((f 'a') True) "hello"
-- The type of (f 'a') is a function that takes a character and
-- returns a Bool -> String -> String
-- The type of (f 'a') True is String -> String

-- So we can write:
f' :: Char -> (Bool -> (String -> String))
f' c b s = c:s ++ show (not b) ++ s

-- This means we can "eta reduce" functions like:
-- f x = e x where e is some expression 
-- can be rewritten as f = e

-- To know when to apply lambda expressions or partial applications etc
-- we can use hlint! Its included with hls in vscode

-- Understanding precedence - avoiding brackets
-- The operator between f and its argument is called the apply operator
-- it's invisible like a lot of other operators in haskell.

-- Everything has a precendence, check :i on the operator
-- precedence is the reason we can write:
-- even x : [] instead of (even x) : [] because the application operator
-- binds tighter than cons

-- We can get around this by using our own apply operator $
-- f $ x = f x, its right associative
-- It says that everything after has a higher precedence than the applic.
-- operator.

-- APPLICATION:
-- Produce a table from a string counting the occurance of words in it.

input :: [Char]
input = "hello clouds hello sky"

-- Step 1, break string into words
-- Step 2, sort the list
-- Step 3, group them by something
-- Step 4, map them 
-- Step 5, map to strings
-- Step 6, unlines
wordCount = unlines
          . map (\(w, n) -> w ++ ": " ++ show n)
          . map (\ws -> (head ws, length ws)) 
          . group 
          . sort 
          . words

-- . = function composition operator, works backwards.
-- applies words, applies sort, then lastly group.
