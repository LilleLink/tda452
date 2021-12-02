import Data.Char (isDigit, isSpace)
import Parsing
import Control.Monad (forever)
--------------------------------------------------------------------------------
-- Parsing: Intro and Overview --
-- Previously - showing expressiong
-- Now        - reading expressions (parsing)
-- read is built in, works for predefined types

-- data Expr = Num Int 

-- Parsing numbers
-- Three cases
    -- One number
    -- Not a number
    -- Starts with a number

-- Parsers should return maybe!
    -- Failure -> Nothing
    -- Success -> Just (x,r)
        -- where x is the thing that we found and r is the rest

-- Alternative |
-- Sequencing
-- Zero or more {}

--------------------------------------------------------------------------------
-- Basic parsing from scratch --
-- String -> Maybe (X, String)
-- Failure -> Nothing
-- Success -> Just(x,r)

type ParserFun a = String -> Maybe (a, String)

num :: ParserFun Integer 
num s = case span isDigit s of -- takes all the digits if it starts with one
    (d:ds, rest) -> Just (read (d:ds),rest)
    _            -> Nothing

-- Span is pretty much like a takewhile dropwhile clause that takes elements
-- from a list if they match the predicate, returns a tuple with that list
-- and the rest of it.

addition0 :: ParserFun Integer 
addition0 s = case num s of
                    Just (n, '+':r) -> case num r of
                                            Just(m, r') -> Just (n+m, r')
                                            _           -> Nothing
                    _               -> Nothing

multiplication0 :: ParserFun Integer 
multiplication0 s = case num s of
                    Just (n, '*':r) -> case num r of
                                            Just(m, r') -> Just (n*m, r')
                                            _           -> Nothing
                    _               -> Nothing

-- calculation ::= addition | multiplication generalization
calculation0 s = case addition0 s of
                    Nothing -> multiplication0 s
                    ok      -> ok

--------------------------------------------------------------------------------
-- A Parsing Libary --
-- Parsing lib has instructions for parsing
-- Monadic

-- Abstract data type "Parser a"

-- sat function, parses a single character

-- | Parsing a digit 
digit' :: Parser Char 
digit' = sat isDigit

-- | Parse a number
number :: Parser Integer
number = read <$> oneOrMore digit

-- | Parse two numbers, separated by + and add them
addition :: Parser Integer 
addition = operation '+' (+)

-- | Parse two numbers, separated by * and mult them
multiplication :: Parser Integer 
multiplication = operation '*' (*)

-- If one line fails the whole thing returns nothing

-- | Generalization
operation c op = do
    n <- number
    char c --sat (=='+')
    m <- number
    return (m `op` n)

calculation :: Parser Integer 
calculation = addition <|> multiplication -- try add, fail? then mult

--------------------------------------------------------------------------------
-- Expression Parser version 1 --

data Expr = Num Integer 
        |   Add Expr Expr
        |   Mul Expr Expr
        deriving (Eq,Show)

-- Gramar
-- expr ::= term {"+" term}.
-- term ::= factor {"*" factor}.
-- factor ::= number | "(" expr ")".

expr, term, factor :: Parser Expr

expr = do
    t <- term
    ts <- zeroOrMore adds
    return $ foldl Add t ts 
        
    where adds = do
            char '+'
            term

term = do 
    t <- factor
    ts <- zeroOrMore (do char '*'; factor) -- same as where above
    return $ foldl Mul t ts 

factor = Num <$> number
    <|> do 
        char '('
        e <- expr
        char ')'
        return e

--------------------------------------------------------------------------------
-- Expression Parser refactored --

-- lib func
chain' :: Parser item -> Parser sep -> Parser [item]
chain' item sep = do -- item <:> zeroOrMore (sep *> item)
    i <- item 
    is <- zeroOrMore (sep *> item)
    return (i:is)

-- refactoring using above

expr', term', factor' :: Parser Expr

expr' = foldl1 Add <$> chain'  term  (char '+')
term' = foldl1 Mul <$> chain' factor (char '*')

factor' = Num <$> number <|> char '(' *> expr <* char ')'
-- Parsing the left bracket, throw away, parse expression, parse right bracket
-- and throw it away.

-- Simple example --

main :: IO ()
main = do putStrLn "Calculator"
          forever readEvalPrint

readEvalPrint :: IO ()
readEvalPrint = do
    putStr "Calculate > "
    s <- getLine
    let s' = filter (not.isSpace) s
    case parse expr s' of
        Just (ans, "") -> print $ eval ans
        _              -> putStrLn "Invalid expression"

eval :: Expr -> Integer 
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a + eval b