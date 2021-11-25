import Data.Char ( toUpper )
-------------------------------------------------------------------------------
-- What is lazy evaluation? --

-- Example:
-- Very slow when n -> inf
expensive :: Integer -> Integer 
expensive n 
    | n <= 1 = 1
    | otherwise = expensive (n-1) + expensive (n-2)

-- If we try to compute, it takes so long
-- But if we try to compute the length of the list for the first 100 elements
-- one would think that it would take ages, but it doesnt need to compute the
-- values, only the length. Therefore it returns 100 right away. Lazy!

-- Can be understood in three terms:
-- Rewriting
    -- Expressions are reduced.
    -- E.g. additions 1+1 are reduced to 2
    -- Unfolding are also done, "unfolds" the left hand side of an expression
    -- For example head (e1:e2) is by definition e1.
    -- Both these are "redexes" - a redex

    -- Haskell rewrites "outside-in" -- "call-by-name/call by name"
    -- head (1+1):(2+2):[] -> 1+1
    -- starts att the left, outmost and in.

    -- Most languages use eager evaluation, or "inside-out", or "call-by value"
    -- evaluation. This is also true for most functional languages! 
    -- F#, ML Erlang, Lisp, Scheme and Scala

    -- Sometimes call by name is better than call by value.
    -- But sometimes worse.
    -- Worse: double x = x + x -> double expensive = expensive + expensive
    -- But haskell gets around this by sharing!
    -- Haskell would unfold the double redex (e.g.) but wont make two copies!
    -- It will create a graph structure that points to one expensive 
    -- computation, so when we have done the first one, the second one will 
    -- already point to the value!
    -- lazy evaluation = call by name + sharing = call by need
    -- "graph reduction"

    -- Extra example of sharing:
    -- twinPrimes = filter twins (zip primes (tail primes))
        -- where twins (x,y) = y == x + 2
    -- "primes" will only be calculated once!

-- Rewriting order - unfolding?

-- Sharing 
    -- See bottom of rewriting

-------------------------------------------------------------------------------
-- Lazy Style --

-- Computation definition and actual computation of the value are separate.
-- Side effects dont work well with this, we cant predict where comp. happens!

-- Example: the Sudoku lab
-- We implement "backtracking"
-- Solve' computes all valid solutions, but only takes the first one.
-- Blank solution in a blink of an eye!

-- Because of laziness we can operate with infininte data structures.
-- We can implement a fuction that gives a list of ALL the prime numbers.
-- We can manipulate that list with take, takeWhile and dropWhile
-- without having to evaluate the entire list!

-- Prelude functions infinite lists make sense on
-- iterate - applies a function to a number and then the result infinitely
-- repeat - just repeats an argument infinitely
-- cycle - repeats a list over and over infinitely

-------------------------------------------------------------------------------
-- Lazy IO --
dict :: [Char]
dict = "/usr/share/dict/words"

firstWord :: FilePath -> IO ()
firstWord f = do
    c <- readFile f
    let c' = head . lines $ c
    putStrLn c'

lastWord :: FilePath -> IO ()
lastWord f = do
    c <- readFile f
    let c' = last $ lines c
    putStrLn c'

-- The first one will only read the first word of the file
-- The last one has to read it all!
-- Big difference in runtime (still small).
-- Haskell knows that it only needs one word.

-- Common pattern - interact
-- Use a function as a pipeline processor
-- connect stdin to the input and stdout to the output
-- Take the stream and transform it with f :: String -> String
-- Called interact :: (String -> String) -> IO ()
-- Lazy evaluation means we can se output before all of the input.

-- Interact commonater makes the function lazy!
encodeLines :: IO ()
encodeLines = interact $ unlines . map urlEncode . lines
    where urlEncode = undefined -- some lib function I dont have.

-------------------------------------------------------------------------------
-- Lazy Data Structures --
-- Not only lists!

-- For example:
data Labarynth = Crossroad String Labarynth Labarynth

what (Crossroad place _ _) = place
right (Crossroad _ _ right) = right 
left (Crossroad _ left _) = left

-- How do we build it?
-- Going in circles! :)
-- Making cycles in a graph.

-------------------------------------------------------------------------------
-- Tail Recursion --
-- Controlling laziness
-- Haskell includes controlling features for laziness.
-- We control when something gets evaluated
-- Purposed for performance tuning.
-- Hard to get right!

-- Sometimes we might get stack overflow if we sum for example to 100000000

sum' [] = 0
sum' (x:xs) = x + sum' xs -- not tail recursive! We are adding x!

-- All languages has this problem, but how do we fix it?
-- Right now we are forced to 1 + (2 + (3 + ..., items are put on the stack
-- over and over, makes memory go bust.

-- To solve this we use tail recursion
-- Tail recursion is when the recursive call is the answer, no adding x like
-- before.

sum'' = s 0 -- extra parameter "sum so far"
    where s acc [] = acc
          s acc (n:ns) = s (n+acc) ns

-- We use an accumulator, has an extra accumulating parameter
-- Stores the sum so far. 

-- Worth noting - prevents us from lazily using a list result!
-- Since it stops us from producing the result until its completely finished.
-- For example , we cannot use the result of reverse until we have gotten to 
-- the end of the list!

-- This is essentially folding from the left
-- foldl (+) 0

-- sum'' is better in time, but not in space

-------------------------------------------------------------------------------
-- Controlling laziness with seq --
-- With the sum'' definition, we still use linear space :(
-- We have only changed the procedure to:
-- s 0 [1..million]
-- s (0+1) [1..million]
-- ... etc. We dont compute the second argument until we are done.

-- We use seq for this.
-- Forces first argument to be evaluated, although not neccessarily needed.

-- There is a strict application operator "$!"
-- f $! x = x `seq` f x

-- The ghc -O flag finds out where its save to insert seq, which optimises
-- the code, hence the O letter for optimisation.

-- We redefine sum:
sum''' = s 0
    where s acc [] = acc
          s acc (n:ns) = acc `seq` s (n+acc) ns

-- About double the speed, more visible when we start to run out of space.
-- This one will do garbage collection, the previous will crash!

-- We can use strict tail recursion by using foldl'
-- Destroys the laziness, but more space efficient.

-------------------------------------------------------------------------------
-- Problems with lazy IO --

shoutify :: FilePath -> IO String 
shoutify f = do
    contents <- readFile f
    let shout = map toUpper contents
    writeFile f shout
    return shout

-- Shouts out the content of the file (converts to all caps) and writes to the
-- file again.

-- One would think that we are reading the contents, manipulating, then writing
-- sequentially, but we dont because of lazy evaluation.

-- We cannot fix this with "seq", since it is also lazy...
-- It forces evaluation, but only as far as the outermost constructor
-- This is called whnf, weak head-normal form.

-- We can trick the fucntion to read the file fully first though, by setting
-- "last" / "length" in front of shout before writeFile, so that it has to
-- compute all of it first.

-- Strict IO --
-- We can also use strict IO to force overriding of lazy evaluation
-- strictReadFile from System.IO.Strict.readFile


-- Conclusion --
-- Controlling evaluation for parrallellism
-- Since haskell is pure we should be able to compile programs using n-cores
-- purity implies that computations can be reordered without changing result.
-- ... in theory.
-- In practice this is hard. We can help it by using pseq.
-- pseq -> Strict promise of left to right evaluation, compiler will not touch.
-- par -> maybe evanluates left then right, but possibly in parrallell.

-- These 2 functions are in the Control.Parallel package.


