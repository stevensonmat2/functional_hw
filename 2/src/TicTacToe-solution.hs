module TicTacToe where

import Data.List (permutations)
import GHC.Generics

data AB where
  A :: AB
  B :: AB -> AB
  deriving Eq

data CD where
  C :: (AB, AB) -> CD
  D :: (E, CD) -> CD
  deriving Eq

data E where
  E :: (AB, CD) -> E
  deriving Eq

practice1 :: AB
practice1 = undefined

practice2 :: AB
practice2 = undefined

practice3 :: CD
practice3 = undefined

practice4 :: CD
practice4 = undefined

practice5 :: E
practice5 = undefined

data Player where
  X :: Player
  O :: Player
  deriving Eq

opponent :: Player -> Player
opponent X = O
opponent O = X

playerToString :: Player -> String
playerToString X = "X"
playerToString O = "O"

instance Show Player where show = playerToString

type Line = (Maybe Player, Maybe Player, Maybe Player)

data Board where
  Board :: (Line, Line, Line) -> Board
  deriving Eq

boardToString :: Board -> String
boardToString (Board (l0, l1, l2)) =
  "Board\n" ++
    "  ( " ++ show l0 ++ "\n" ++
    "  , " ++ show l1 ++ "\n" ++
    "  , " ++ show l2 ++ "\n" ++
    "  )"

instance Show Board where show = boardToString

emptyBoard :: Board
emptyBoard =
  Board
    ( (Nothing, Nothing, Nothing)
    , (Nothing, Nothing, Nothing)
    , (Nothing, Nothing, Nothing)
    )

testBoard :: Board
testBoard =
  Board
    ( (Nothing, Nothing, Just X)
    , (Just O, Just O, Just X)
    , (Just O, Nothing, Nothing)
    )

data LineIndex where
  C0 :: LineIndex
  C1 :: LineIndex
  C2 :: LineIndex
  deriving Eq

allLineIndices :: [LineIndex]
allLineIndices = [C0, C1, C2]

lineIndexToString :: LineIndex -> String
lineIndexToString C0 = "C0"
lineIndexToString C1 = "C1"
lineIndexToString C2 = "C2"

instance Show LineIndex where show = lineIndexToString

indexLine :: (LineIndex, Line) -> Maybe Player
indexLine (C0, (x0, x1, x2)) = x0
indexLine (C1, (x0, x1, x2)) = x1
indexLine (C2, (x0, x1, x2)) = x2

updateLine :: (LineIndex, Player, Line) -> Line
updateLine (C0, p, (x0, x1, x2)) = (Just p, x1, x2)
updateLine (C1, p, (x0, x1, x2)) = (x0, Just p, x2)
updateLine (C2, p, (x0, x1, x2)) = (x0, x1, Just p)

type BoardIndex = (LineIndex, LineIndex)

axisBoardIndices :: (LineIndex, [LineIndex]) -> [BoardIndex]
axisBoardIndices (i, []) = []
axisBoardIndices (i, (j : js)) = (i, j) : axisBoardIndices (i, js)

someBoardIndices :: ([LineIndex], [LineIndex]) -> [BoardIndex]
someBoardIndices ([], js) = []
someBoardIndices (i : is, js) = axisBoardIndices (i, js) ++ someBoardIndices (is, js)

allBoardIndices :: [BoardIndex]
allBoardIndices = someBoardIndices (allLineIndices, allLineIndices)

indexBoard :: (BoardIndex, Board) -> Maybe Player
indexBoard ((C0, i), Board (l0, l1, l2)) = indexLine (i, l0)
indexBoard ((C1, i), Board (l0, l1, l2)) = indexLine (i, l1)
indexBoard ((C2, i), Board (l0, l1, l2)) = indexLine (i, l2)

updateBoard :: (BoardIndex, Player, Board) -> Board
updateBoard ((C0, i), p, Board (l0, l1, l2)) = Board (updateLine (i, p, l0), l1, l2)
updateBoard ((C1, i), p, Board (l0, l1, l2)) = Board (l0, updateLine (i, p, l1), l2)
updateBoard ((C2, i), p, Board (l0, l1, l2)) = Board (l0, l1, updateLine (i, p, l2))

filterEmptyIndices :: ([BoardIndex], Board) -> [BoardIndex]
filterEmptyIndices ([], b) = []
filterEmptyIndices (i : is, b) =
  if indexBoard (i, b) == Nothing then
    i : filterEmptyIndices (is, b)
  else
    filterEmptyIndices (is, b)

allEmptyIndices :: Board -> [BoardIndex]
allEmptyIndices b = filterEmptyIndices (allBoardIndices, b)

type WinningLine = (BoardIndex, BoardIndex, BoardIndex)

allWinningLines :: [WinningLine]
allWinningLines =
  [ ((C0,C0), (C0,C1), (C0,C2))
  , ((C1,C0), (C1,C1), (C1,C2))
  , ((C2,C0), (C2,C1), (C2,C2))
  , ((C0,C0), (C1,C0), (C2,C0))
  , ((C0,C1), (C1,C1), (C2,C1))
  , ((C0,C2), (C1,C2), (C2,C2))
  , ((C0,C0), (C1,C1), (C2,C2))
  , ((C0,C2), (C1,C1), (C2,C0))
  ]

checkOneWinningLine :: (WinningLine, Player, Board) -> Bool
checkOneWinningLine ((i0, i1, i2), p, b) =
  indexBoard (i0, b) == Just p &&
  indexBoard (i1, b) == Just p &&
  indexBoard (i2, b) == Just p

checkSomeWinningLines :: ([WinningLine], Player, Board) -> Bool
checkSomeWinningLines ([], p, b) = False
checkSomeWinningLines (w : ws, p, b) =
  checkOneWinningLine (w, p, b) || checkSomeWinningLines (ws, p, b)

hasWon :: (Player, Board) -> Bool
hasWon (p, b) = checkSomeWinningLines (allWinningLines, p, b)

simulateGame :: ([BoardIndex], Player, Board) -> Int
simulateGame ([], p, b) = 0
simulateGame (i : is, p, b) =
  if hasWon (p, b) then
    1
  else if hasWon (opponent p, b) then
    -1
  else
    negate (simulateGame (is, opponent p, updateBoard (i, p, b)))

simulateSomeGames :: ([[BoardIndex]], Player, Board) -> [Int]
simulateSomeGames ([], p, b) = []
simulateSomeGames (is : iss, p, b) =
  simulateGame (is, p, b) : simulateSomeGames (iss, p, b)

simulateAllGames :: (Player, Board) -> [Int]
simulateAllGames (p, b) =
  simulateSomeGames (permutations (allEmptyIndices b), p, b)

rateMoveOutcome :: (BoardIndex, Player, Board) -> Int
rateMoveOutcome (i, p, b) =
  negate (sum (simulateAllGames (opponent p, updateBoard (i, p, b))))

findBestMoveInList :: ([BoardIndex], Player, Board) -> Maybe (BoardIndex, Int)
findBestMoveInList ([], p, b) = Nothing
findBestMoveInList (thisMove : otherMoves, p, b) =
  let thisRating = rateMoveOutcome (thisMove, p, b) in
    case findBestMoveInList (otherMoves, p, b) of
      Nothing -> Just (thisMove, thisRating)
      Just (otherBestMove, otherBestRating) ->
        if thisRating > otherBestRating then
          Just (thisMove, thisRating)
        else
          Just (otherBestMove, otherBestRating)

findBestMove :: (Player, Board) -> Maybe (BoardIndex, Int)
findBestMove (p, b) = findBestMoveInList (allEmptyIndices b, p, b)

