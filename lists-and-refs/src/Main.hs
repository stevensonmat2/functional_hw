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
populateRef ref = do
    n <- readInt
    writeIORef ref (n + 1)


main :: IO ()
main = do
    ref <- createreadIntRef
    populateRef ref
    x <- readIORef ref
    print x