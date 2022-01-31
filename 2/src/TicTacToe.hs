-- This module implements some algorithms for playing a game of tic-tac-toe:
-- the most important functions are "checkIfWon", which checks whether a player
-- has won the game, and "findBestMove", which finds the best possible move
-- that a player can make using a very crude little AI algorithm.

-- If you're unfamiliar with the game of tic-tac-toe, take a few minutes to
-- read the Wikipedia page for it and become familiar with the rules; it's a
-- very simple game. Crucially, note that the game *always ends in a tie* if
-- both players play perfectly. This means that at least in principle, we
-- should be able to build a tic-tac-toe AI that *never loses*. In fact, our
-- "findBestMove" function has this property.

-- After a little warm-up practice exercise, you will be filling in some parts
-- of these algorithms. Don't worry - this won't require you to have any
-- knowledge of AI! The code you'll be working on will build directly on the
-- concepts that we covered in the first assignment, specifically the technique
-- of using pure recursion to process singly-linked lists.

-- The app/Main.hs file contains the UI code for playing a game of tic-tac-toe
-- with one human player and one AI player. You are not expected to read or
-- understand the Main.hs code, but you're welcome to ask questions about it!

-- The tests/Spec.hs file contains a test suite for the functions you'll be
-- working on in this code. You are also not expected to read or understand
-- this test code, but you're also welcome to ask about it! Read the README for
-- more information on how to run the tests and interpret test failures.

module TicTacToe where

-- Unlike in our first warm-up Haskell project from lecture, we *are* getting
-- all of the "automatic imports" (the Prelude) in this file. Here we import
-- one *additional* function that is not imported for us automatically: a
-- function to get all permutations of a list. This will come up later.
import Data.List (permutations)


-- ================
-- EXERCISE 1 START
-- ================

-- Before we start working on some actual algorithms, here's an exercise to
-- double-check that you know how to construct values in Haskell using
-- constructor functions. Review the lecture notes and recordings if you need
-- help with this exercise!

-- Here are some little made-up data types for this exercise. None of these are
-- "real-world" types, they're just for practice reading and understanding
-- Haskell data type definitions.

data AB where
  A :: AB
  B :: AB -> AB

data CD where
  C :: (AB, AB) -> CD
  D :: (E, CD) -> CD

-- Note that while a constructor is not *required* to have the same name as its
-- type, it is *allowed* to have the same name as its type, as in this example.
-- This has no special meaning; the "E" constructor would work the same way if
-- it was renamed to "F" or any other name. When a type only has one
-- constructor, it is common practice to give that constructor the same name as
-- the data type.
data E where
  E :: (AB, CD) -> E

-- Replace each use of "undefined" with an expression of the correct type in
-- the definitions below, so that each definition passes typechecking.

-- To be clear, these definitions don't *do* anything in particular: they're
-- just for practice working with the typechecker. There's no real "runnable"
-- code in this exercise.

-- You may ONLY use the A, B, C, D, and E constructors in your code for this
-- exercise: no other functions or values.

-- Your definition for practice1 must be *different* than your definition for
-- practice2, and your definition for practice3 must be different than your
-- definition for practice4.

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

-- ==============
-- EXERCISE 1 END
-- ==============


-- Now, on to our tic-tac-toe code. First we have a data type for the two
-- players, which are traditionally represented in tic-tac-toe as X and O.
data Player where
  X :: Player
  O :: Player
  deriving (Eq, Show)

-- The "deriving (Eq, Show)" part of this definition causes Haskell to
-- magically generate some code for us: specifically, it allows us to use the
-- == operator to compare two Player values for equality, and to use the "show"
-- function to convert a Player to a String. It also tells the REPL to use this
-- automatically-generated "show" function when printing Player values.

-- Here's our first real function in this file: we will have many uses for
-- getting the opponent of a given player.
opponent :: Player -> Player
opponent X = O
opponent O = X

-- This is a new form of type definition that we haven't seen before: note that
-- we're using the "type" keyword instead of the "data" keyword. This is like a
-- "typedef" in C/C++, or a "type synonym" in many other languages: we're
-- declaring that the name "Line" is a *synonym* for the 3-tuple type on the
-- right-hand side, which means it has the **EXACT** same meaning as the
-- right-hand side. IT IS NOT CORRECT TO SAY THAT A Line *CONTAINS* A 3-TUPLE:
-- a Line *IS* a 3-tuple. Make sure you understand this distinction!
type Line = (Maybe Player, Maybe Player, Maybe Player)

-- For example, here's a sample Line value.
sampleLine :: Line
sampleLine = (Nothing, Just X, Just X)

-- Since a type synonym has the *exact* same meaning as the right-hand side of
-- its definition, we don't need to tell Haskell how to compare two Lines for
-- equality or how to convert a Line to a string: it already knows how to do
-- all of these things for the Player type, the Maybe type, and 3-tuple types,
-- so it already knows how to do them for the Line type.

-- A Line represents a one-dimensional line on our 3x3 tic-tac-toe board; a
-- Board consists of three Lines. Note that we're using "data" instead of
-- "type" here! It is not correct to say that a Board *is* a 3-tuple; a Board
-- *contains* a 3-tuple.
data Board where
  Board :: (Line, Line, Line) -> Board
  deriving Eq

-- Defining the Board type with "data" is not strictly necessary for our
-- algorithm: we could have used "type Board = (Line, Line, Line)" instead.
-- We do it this way because using "data" allows us to customize how a Board is
-- *displayed* in the REPL, which we can't do with a "type" definition. This is
-- also why we aren't "deriving Show" in the Board type definition: we *want*
-- to manually define how to convert a Board to a string, so that we can
-- customize how a board is displayed in the REPL.

-- In particular, if we use a "type" definition, the REPL will print all of the
-- Board data on a single line of text. It's much more convenient for debugging
-- if we split it up across multiple lines of text, as we do here.
boardToString :: Board -> String
boardToString (Board (l0, l1, l2)) =
  "Board\n" ++
    "  ( " ++ show l0 ++ "\n" ++
    "  , " ++ show l1 ++ "\n" ++
    "  , " ++ show l2 ++ "\n" ++
    "  )"

-- Remember, this is the magic that tells the REPL how to "show" the result of
-- a computation that produces a Board value. We need to manually invoke this
-- magic for Board because it's defined with "data" and its definition does not
-- have a "deriving Show" clause.
instance Show Board where show = boardToString

-- A Board is a 3x3 grid of cells, each of which may be "empty" (Nothing) or
-- contain a player mark (Just X or Just O). This represents the state of a
-- tic-tac-toe game.

-- This is the state of the board at the start of a game of tic-tac-toe.
emptyBoard :: Board
emptyBoard =
  Board
    ( (Nothing, Nothing, Nothing)
    , (Nothing, Nothing, Nothing)
    , (Nothing, Nothing, Nothing)
    )

-- Here's a sample board with an in-progress game state.
testBoard :: Board
testBoard =
  Board
    ( (Nothing, Nothing, Just X)
    , (Just O, Just O, Just X)
    , (Just O, Nothing, Nothing)
    )

-- Now that we have our basic types to represent our game state, we need a way
-- to access the value at some location on a Board. We start by defining the
-- type of Line indices: every Line has exactly three indices, which we'll
-- label C0, C1, and C2.
data LineIndex where
  C0 :: LineIndex
  C1 :: LineIndex
  C2 :: LineIndex
  deriving (Eq, Show)

-- If we have a LineIndex, we can use it to index into a Line and get back a
-- Maybe Player value. Note that there is no possibility of an out-of-bounds
-- index, because our LineIndex type only has these three values; we return a
-- Maybe Player because that's precisely what's stored in the Line.
indexLine :: (LineIndex, Line) -> Maybe Player
indexLine (C0, (x0, x1, x2)) = x0
indexLine (C1, (x0, x1, x2)) = x1
indexLine (C2, (x0, x1, x2)) = x2

-- To play a game of tic-tac-toe, we'll also need a way to *modify* a Board,
-- which we'll define in terms of modifying a particular Line. As usual, since
-- we're in a pure environment, we output a modified *copy* of the input
-- instead of changing the input value itself. We take a Player instead of a
-- Maybe Player as input here because in the game of tic-tac-toe, it is never
-- valid to *erase* a mark on the board: we only ever *add* marks onto the
-- board until the game is over.
updateLine :: (LineIndex, Player, Line) -> Line
updateLine (C0, p, (x0, x1, x2)) = (Just p, x1, x2)
updateLine (C1, p, (x0, x1, x2)) = (x0, Just p, x2)
updateLine (C2, p, (x0, x1, x2)) = (x0, x1, Just p)

-- A BoardIndex is simply a pair of LineIndex values: the first LineIndex is
-- the row index (from top to bottom), and the second LineIndex is the column
-- index (from left to right).
type BoardIndex = (LineIndex, LineIndex)

-- Here we use our indexLine function to access a particular cell on a Board.
indexBoard :: (BoardIndex, Board) -> Maybe Player
indexBoard ((C0, i), Board (l0, l1, l2)) = indexLine (i, l0)
indexBoard ((C1, i), Board (l0, l1, l2)) = indexLine (i, l1)
indexBoard ((C2, i), Board (l0, l1, l2)) = indexLine (i, l2)


-- ================
-- EXERCISE 2 START
-- ================

-- Implement the "updateBoard" to modify the input Board by placing the given
-- Player mark at the given BoardIndex. You do not need to check what value is
-- already at the given BoardIndex; just replace whatever's there with the
-- given Player value.

-- For example:
--
-- "updateBoard ((C0,C1), X, testBoard)" should produce
--   Board
--     ( (Nothing, Just X, Just X)
--     , (Just O, Just O, Just X)
--     , (Just O, Nothing, Nothing)
--     )
--
-- "updateBoard ((C1,C1), X, testBoard)" should produce
--   Board
--     ( (Nothing, Nothing, Just X)
--     , (Just O, Just X, Just X)
--     , (Just O, Nothing, Nothing)
--     )

-- In general, for any "i :: BoardIndex", "p :: Player", and "b :: Board",
-- this equality should always hold:
--   indexBoard (i, updateBoard (i, p, b)) == Just p
-- This is what the automated test suite checks for.

-- You will need to *add* arguments to the left-hand side of the = sign in this
-- definition and *replace* the "undefined" on the right-hand side of the =
-- sign. You may use multiple cases ("pattern matching on the left of the =").

-- The text of your solution must contain *at most one* use of each of the C0,
-- C1, and C2 constructors. (You may call other functions that use these
-- constructors more than once: the requirement is on the text of the code
-- that *you write*.)

-- Do not modify the type of "updateBoard", and do not add any additional code
-- outside of the "updateBoard" definition.

updateBoard :: (BoardIndex, Player, Board) -> Board
updateBoard = undefined

-- ==============
-- EXERCISE 2 END
-- ==============


-- How do we know when a player has won a game of tic-tac-toe?

-- A WinningLine is a sequence of three spaces that a player can win with.
type WinningLine = (BoardIndex, BoardIndex, BoardIndex)

-- These are all eight different ways to win a game of tic-tac-toe.
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

-- This function checks whether a player has won by *some particular*
-- WinningLine: if it returns False, it doesn't mean they *haven't* won the
-- game, it just means they haven't won *in this particular way*.
checkOneWinningLine :: (WinningLine, Player, Board) -> Bool
checkOneWinningLine ((i0, i1, i2), p, b) =
  indexBoard (i0, b) == Just p &&
  indexBoard (i1, b) == Just p &&
  indexBoard (i2, b) == Just p

-- This function checks whether a player has won by some *list* of
-- WinningLines, which may or may not contain all possible WinningLines.
checkSomeWinningLines :: ([WinningLine], Player, Board) -> Bool
checkSomeWinningLines ([], p, b) = False
checkSomeWinningLines (w : ws, p, b) =
  checkOneWinningLine (w, p, b) || checkSomeWinningLines (ws, p, b)

-- Finally, to check if a player has won, we just check all WinningLines.
checkIfWon :: (Player, Board) -> Bool
checkIfWon (p, b) = checkSomeWinningLines (allWinningLines, p, b)


-- Now that we've got our code to do basic Board queries and manipulations,
-- it's time to start building our AI. Since tic-tac-toe is such a simple game,
-- we can write a perfectly decent tic-tac-toe AI without thinking about any
-- actual AI coding techniques: we'll just have our AI consider *every*
-- possible sequence of moves, rate each one based on whether it leads to a win
-- or a loss, and pick the best move based on this rating.

-- First, we'll need a way to find all *possible* moves on a given board. For
-- tic-tac-toe, this means we need a function to find all of the empty
-- (Nothing) spaces on a given board. We'll start by generating *all* indices
-- on the board, and then we'll *filter* that list of indices down to just the
-- empty indices.

-- This function generates all indices on a board along a given *row*. Try it
-- in the REPL to get a feel for how it works!
rowBoardIndices :: (LineIndex, [LineIndex]) -> [BoardIndex]
rowBoardIndices (i, []) = []
rowBoardIndices (i, (j : js)) = (i, j) : rowBoardIndices (i, js)

-- This function generates all indices on a board across *multiple* given rows.
someBoardIndices :: ([LineIndex], [LineIndex]) -> [BoardIndex]
someBoardIndices ([], js) = []
someBoardIndices (i : is, js) = rowBoardIndices (i, js) ++ someBoardIndices (is, js)

-- Here are all of our LineInddex values, in order.
allLineIndices :: [LineIndex]
allLineIndices = [C0, C1, C2]

-- Finally, we generate all possible BoardIndex values by generating all values
-- in each row, for all rows.
allBoardIndices :: [BoardIndex]
allBoardIndices = someBoardIndices (allLineIndices, allLineIndices)

-- Wait - why didn't we just use this obvious definition?
--   allBoardIndices :: [BoardIndex]
--   allBoardIndices =
--     [ (C0,C0), (C0,C1), (C0,C2)
--     , (C1,C0), (C1,C1), (C1,C2)
--     , (C2,C0), (C2,C1), (C2,C2)
--     ]
-- In a "real-world" tic-tac-toe program, we probably would have. But this is a
-- learning exercise! One benefit of our more abstract definition is that it's
-- easy to scale to larger board dimensions: we *only* have to modify the
-- allLineIndices definition. This is silly in the context of tic-tac-toe, of
-- course, but it represents the idea of *planning for scalability*, which is
-- one big motivation of functional programming.


-- ================
-- EXERCISE 3 START
-- ================

-- Implement the "filterEmptyIndices" function to return *only* the elements of
-- the given input list which correspond to empty (Nothing) spaces on the given
-- Board, in the same order as in the original input list.

-- For example:
--
-- "filterEmptyIndices ([(C0,C0), (C0,C2), (C1,C0)], testBoard)" should produce
--   [(C0,C0)]
--
-- "filterEmptyIndices ([(C2,C0), (C0,C0), (C0,C1)], testBoard)" should produce
--   [(C0,C0), (C0,C1)]
--
-- "filterEmptyIndices ([], testBoard)" should produce
--   []

-- You will need to *add* arguments to the left-hand side of the = sign in this
-- definition and *replace* the "undefined" on the right-hand side of the =
-- sign. You may use multiple cases ("pattern matching on the left of the =").

-- The text of your solution must contain *no uses* of the C0, C1, and C2
-- constructors. (You may call other functions that use these constructors: the
-- requirement is on the text of the code that *you write*.)

-- Do not modify the type of "filterEmptyIndices", and do not add any
-- additional code outside of the "filterEmptyIndices" definition.

filterEmptyIndices :: ([BoardIndex], Board) -> [BoardIndex]
filterEmptyIndices = undefined

-- ==============
-- EXERCISE 3 END
-- ==============


-- Now to find all possible moves on a given Board, we just have to take the
-- list of all possible indices and filter it down to the indices representing
-- empty spaces.
allEmptyIndices :: Board -> [BoardIndex]
allEmptyIndices b = filterEmptyIndices (allBoardIndices, b)

-- To simulate a particular sequence of moves, we pass in the moves as a list
-- of BoardIndexes and alternate players: player "p" makes the first move,
-- "opponent p" makes the second move, "p" makes the third move, and etc. If
-- the sequence of moves leads to a win for "p", we return 1; if it leads to a
-- loss for "p", we return -1; and if it leads to a tie or fails to finish the
-- game at all, we return 0. This rating indicates how good or bad the given
-- sequence of moves is for player "p".
simulateGame :: ([BoardIndex], Player, Board) -> Int
simulateGame ([], p, b) = 0
simulateGame (i : is, p, b) =
  if checkIfWon (p, b) then
    1
  else if checkIfWon (opponent p, b) then
    -1
  else
    negate (simulateGame (is, opponent p, updateBoard (i, p, b)))

-- Expanding our AI a little, this function considers a *list* of possible
-- sequences of moves, and returns a rating for *each* sequence of moves.
simulateSomeGames :: ([[BoardIndex]], Player, Board) -> [Int]
simulateSomeGames ([], p, b) = []
simulateSomeGames (is : iss, p, b) =
  simulateGame (is, p, b) : simulateSomeGames (iss, p, b)

-- Now, to have our AI consider *all possible* sequences of moves, we simply
-- consider all permutations of the list of empty indices: this gets us a list
-- of ratings for *every possible* outcome from this point in the game. The sum
-- of this list represents how good or bad this Board is for the given Player.
simulateAllGames :: (Player, Board) -> [Int]
simulateAllGames (p, b) =
  simulateSomeGames (permutations (allEmptyIndices b), p, b)

-- Finally, to get a rating for how *good* a particular *move* is for a given
-- player, we check how *bad* the *board after that move* is for the *opponent*
-- of the given player. This is subtle, and it's really the only piece of
-- "serious" AI code in this codebase; it works because tic-tac-toe is a
-- "zero-sum game", meaning that what is bad for one player is equally good for
-- the other player. Larger results indicate better moves.
rateMoveOutcome :: (BoardIndex, Player, Board) -> Int
rateMoveOutcome (i, p, b) =
  negate (sum (simulateAllGames (opponent p, updateBoard (i, p, b))))


-- ================
-- EXERCISE 4 START
-- ================

-- Implement the "findBestMoveInList" function to return the index and rating
-- of the best move for the given player in the given BoardIndex list,
-- according to the result of "rateMoveOutcome". If the given list is empty,
-- your implementation should return Nothing.

-- You will need to complete exercises 2 and 3 before attempting this one,
-- since "rateMoveOutcome" depends on your solution to those exercises.

-- For example:

-- "findBestMoveInList ([(C0,C0), (C0,C1), (C2,C2)], X, testBoard)" should produce
--   Just ((C2,C2), 6)
-- because
--   rateMoveOutcome ((C0,C0), X, testBoard) = 4
--   rateMoveOutcome ((C0,C1), X, testBoard) = 1
--   rateMoveOutcome ((C2,C2), X, testBoard) = 6

-- "findBestMoveInList ([(C1,C1), (C2,C2)], O, testBoard)" should produce
--   Just ((C2,C2), 4)
-- because
--   rateMoveOutcome ((C1,C1), O, testBoard) = -10
--   rateMoveOutcome ((C2,C2), O, testBoard) = 4

-- "findBestMoveInList ([], O, testBoard)" should produce
--   Nothing
-- because the input list is empty

-- You will need to *add* arguments to the left-hand side of the = sign in this
-- definition and *replace* the "undefined" on the right-hand side of the =
-- sign. You may use multiple cases ("pattern matching on the left of the =").

-- The text of your solution must contain *no uses* of the C0, C1, and C2
-- constructors. (You may call other functions that use these constructors: the
-- requirement is on the text of the code that *you write*.)

-- Do not modify the type of "findBestMoveInList", and do not add any
-- additional code outside of the "findBestMoveInList" definition.

findBestMoveInList :: ([BoardIndex], Player, Board) -> Maybe (BoardIndex, Int)
findBestMoveInList = undefined

-- ==============
-- EXERCISE 4 END
-- ==============


-- Finally, to find the best possible move for a player on a given board, we
-- simply find the best move out of the list of all possible moves.
findBestMove :: (Player, Board) -> Maybe (BoardIndex, Int)
findBestMove (p, b) = findBestMoveInList (allEmptyIndices b, p, b)

-- That's it! Check the README to see how to run the tic-tac-toe game; if your
-- implementations are all correct, the AI should be unbeatable!
