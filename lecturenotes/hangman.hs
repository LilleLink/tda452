import System.Random ( randomRIO )
import Data.List
------------------------------------------------------------
-- Hangman game
-- To run standalone program - has to have main

wordsFile = "/usr/share/dict/words" -- mac osx only
guessLimit = 10

main :: IO()
main =  do
    w <- randomWord -- Takes a randomWord from the dict
    gameLoop w ""

randomWord :: IO String 
randomWord = do
    wlist <- readFile wordsFile
    let ws = words wlist
    n <- randomRIO (0, length ws-1)
    return (ws !! n)

gameLoop :: String -> String -> IO()
gameLoop w g | win = showWin
             | lose = showLose
             | otherwise = 
                do 
                displayStatus
                guesses <- getLine
                gameLoop w (g `union` take lives guesses)
    where
        win = and [c `elem` g | c <- w] -- all (`elem` g) w
        lose = lives <= 0 -- the list difference operator
        lives = guessLimit - length (g \\ w)
        showWin         = putStrLn "You won!" 
        showLose        = putStrLn ("You lost! Word was " ++ w)
        displayStatus   = do
            putStrLn [if c `elem` g then c else '_' | c <- w]
            putStrLn $ "Type your guesses (" ++ show lives ++ " remaining)"

-- IO is an example of a monad