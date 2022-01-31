CS 457/557 FUNCTIONAL PROGRAMMING
ASSIGNMENT 2

Welcome to our second assignment! This is our "getting started with Haskell" assignment: we're not going to be doing anything fancy with Haskell's more advanced functional features yet, but we'll get a bit of practice manipulating data with Haskell's support for pattern matching and recursion. At the end, you'll be able to play tic-tac-toe against an AI that you wrote part of!

This assignment requires an installation of GHC and Cabal to complete. Refer to the lecture notes and recordings if you still need to install these tools.

As mentioned in lecture, you may find it helpful to use the Debug.Trace module to aid your debugging process. If you add an import for Debug.Trace, please make sure to remove the import and any trace calls before submitting your code, to avoid confusing the grading scripts.

To start the assignment, open src/TicTacToe.hs and start reading from the top. Read all the comments carefully and ask if anything is unclear!


Here's a cheat sheet of *terminal* commands that will be helpful:

  Enter the REPL:
    cabal repl

  Compile the project:
    cabal build

  Run the compiled tic-tac-toe program:
    cabal exec hw2

  Run the test suite:
    cabal test --test-show-details=direct

  Rerun a specific test:
    cabal test --test-show-details=direct --test-option=--match --test-option="/exercise 1/"

Note that the test output will give you incorrect information about how to rerun a specific test; ignore what it says and use the command exactly as written in this cheat sheet.

Keep in mind that the tests exist to tell you *whether* your code is correct, not *how* to write correct code. The test output will often be somewhat unhelpful; if you get a test failure, your first step should be to test out your code manually in the REPL to see where your assumptions have gone wrong.


Here's a cheat sheet of *REPL* commands that will be helpful:

  Load a single module:
    :l TicTacToe

  Reload the current module:
    :r

  Print the result of any expression:
    indexBoard ((C1,C1), testBoard)
 
  Print the type of any expression:
    :t indexBoard ((C1,C1), testBoard)

  Print all information about a single identifier:
    :i indexBoard
  
  Quit the REPL:
    :q


When you're finished with the assignment, submit ONLY your modified TicTacToe.hs file to the Canvas assignment 2 dropbox. PLEASE DO NOT ZIP OR RENAME YOUR FILE.
