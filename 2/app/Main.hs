module Main where

import TicTacToe

promptPlayer :: IO Player
promptPlayer = do
  putStrLn "Which player do you want to be? (X plays first.)"
  putStrLn "Enter X or O:"
  line <- getLine
  case line of
    "X" -> pure X
    "x" -> pure X
    "O" -> pure O
    "o" -> pure O
    _ -> do
      putStrLn "Invalid input, try again."
      promptPlayer

charToLineIndex :: Char -> Maybe LineIndex
charToLineIndex '0' = Just C0
charToLineIndex '1' = Just C1
charToLineIndex '2' = Just C2
charToLineIndex _ = Nothing

promptBoardIndex :: Board -> IO BoardIndex
promptBoardIndex b = do
  putStrLn "Where do you want to move?"
  putStrLn "Enter two digits (0/1/2) separated by a space."
  line <- getLine
  case line of
    [row, ' ', col] ->
      case (charToLineIndex row, charToLineIndex col) of
        (Just i, Just j) ->
          if indexBoard ((i, j), b) == Nothing then
            pure (i, j)
          else do
            putStrLn "That space is already occupied, try again."
            promptBoardIndex b
        _ -> do
          putStrLn "Invalid input, try again."
          promptBoardIndex b
    _ -> do
      putStrLn "Invalid input, try again."
      promptBoardIndex b

boardSpaceString :: Maybe Player -> String
boardSpaceString Nothing = " "
boardSpaceString (Just X) = "X"
boardSpaceString (Just O) = "O"

printBoard :: Board -> IO ()
printBoard
  ( Board
    ( (x00, x01, x02)
    , (x10, x11, x12)
    , (x20, x21, x22)
    )
  ) = do
    putStrLn " 0 0 | 0 1 | 0 2"
    putStrLn
      ( "  "    ++ boardSpaceString x00 ++
        "  |  " ++ boardSpaceString x01 ++
        "  |  " ++ boardSpaceString x02
      )
    putStrLn "     |     |"
    putStrLn "-----------------"
    putStrLn " 1 0 | 1 1 | 1 2"
    putStrLn
      ( "  "    ++ boardSpaceString x10 ++
        "  |  " ++ boardSpaceString x11 ++
        "  |  " ++ boardSpaceString x12
      )
    putStrLn "     |     |"
    putStrLn "-----------------"
    putStrLn " 2 0 | 2 1 | 2 2"
    putStrLn
      ( "  "    ++ boardSpaceString x20 ++
        "  |  " ++ boardSpaceString x21 ++
        "  |  " ++ boardSpaceString x22
      )
    putStrLn "     |     |"

playGame :: (Player, Player, Board) -> IO ()
playGame (humanPlayer, currentPlayer, b) = do
  printBoard b
  if checkIfWon (humanPlayer, b) then
    putStrLn "You win!"
  else if checkIfWon (opponent humanPlayer, b) then
    putStrLn "You lose."
  else if allEmptyIndices b == [] then
    putStrLn "Tie game."
  else do
    i <-
      if currentPlayer == humanPlayer then
        promptBoardIndex b
      else do
        putStrLn "The AI is thinking..."
        case findBestMove (currentPlayer, b) of
          Just (j, _) -> do
            putStrLn "The AI makes a move."
            pure j
          Nothing -> error "AI error: impossible state"
    playGame (humanPlayer, opponent currentPlayer, updateBoard (i, currentPlayer, b))

main :: IO ()
main = do
  p <- promptPlayer
  playGame (p, X, emptyBoard)
