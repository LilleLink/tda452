import System.IO.Error (tryIOError)
import Data.Either
import System.Directory (listDirectory)
-- Exercises from the video

-- copyAll - copies all the contents from a list of files
-- to a single file using mapM
copyAll :: [FilePath] -> FilePath -> IO()
copyAll fromFiles toFile = do
    contents <- mapM readFile fromFiles
    writeFile toFile (unwords contents)

-- forLoop - takes a list of elements, a function that operates from a
-- to an IO instruction, then maps the function on all elelemts in the list
forLoop :: [a] -> (a -> IO()) -> IO()
forLoop as f = do mapM_ f as

-- The last line of a do block must have the same type as 
-- the entire do block.
-- Lines before must be of form pattern <- exp or just exp

-- Do blocks are used to build bigger instructions of smaller ones
-- Where IO a is more of an IO instruction to produce a


-- (1) Explain the error in each of the definitions:
-- readFileSample1: = instead of <-, typerror
-- readFileSample2: take cannot get an IO String
-- readFileInteractive: return (IO String) is wrong, it'll generate an IO (IO String)

-- (2) Write a backupDirectory function using readAllFiles :: IO [(FilePath, String)]
-- and zipFiles :: [(FilePath, String)] -> String
readAllFiles :: IO [(FilePath, String)]
readAllFiles = undefined 

zipFiles :: [(FilePath, String)] -> String
zipFiles = undefined 

backupDirectory :: IO ()
backupDirectory = do
    filesList <- readAllFiles
    writeFile "backup.zip" (zipFiles filesList)

-- (3) kidInCar
kidInCar :: IO()
kidInCar = do 
    putStrLn "Retarded child: Are we there yet?"
    ans <- getLine
    case ans of
        "yes" -> return ()
        _     -> kidInCar
    
kidInCar' :: IO()
kidInCar' = do 
    putStrLn "Retarded child: Are we there yet?"
    ans <- getLine
    check ans
        where check :: String -> IO()
              check ans | ans == "yes" = return ()
                        | otherwise = kidInCar'

-- (4) file operations with error catching
readFile' :: FilePath -> IO String
readFile' fp = do
    contents <- tryIOError (readFile fp)
    case contents of
        Left error -> return "Could not read from file"
        Right str -> return str

writeFile' :: FilePath -> String -> IO ()
writeFile' fp contents = do
    result <- tryIOError (writeFile fp contents)
    case result of
        Left error -> putStr $ show error
        Right _    -> return ()

-- (5) Implement readAllFiles using tryIOError and System.Directory
readAllFiles' :: IO [(FilePath, String)]
readAllFiles' = do
    paths <- listDirectory "."
    contents <- mapM readFile' paths
    return $ zip paths contents


