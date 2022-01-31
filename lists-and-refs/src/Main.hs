module Main where

import ListRefPrelude
import Text.Read
import Data.IORef

para :: String -> String
para a = "(" ++ a ++ ")"

readInt :: IO Int
readInt = do
    x <- getLine
    case readMaybe x of
        Nothing -> do
            putStrLn "bad int"
            readInt
        Just n ->
            pure n


createreadIntRef :: IO (IORef Int)
createreadIntRef = newIORef 0

populateRef :: IORef Int -> IO ()
populateRef = do
    n <- readInt
    writeIORef ref n

main :: IO ()
main = do
    x <- getLine
    y <- readInt
    print (y + 1)
    putStrLn (para(reverse(x)))
    putStrLn "goodbye"

