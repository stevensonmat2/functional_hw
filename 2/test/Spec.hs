module Main where

import Data.Function
import Data.Maybe
import GHC.Generics
import Test.Hspec.Core.Spec
import Test.Hspec.Core.QuickCheck
import Test.Hspec.Core.Runner
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import TicTacToe

deriving instance Eq AB
deriving instance Show AB

deriving instance Eq CD
deriving instance Show CD

deriving instance Eq E
deriving instance Show E

deriving instance Generic Player
deriving via GenericArbitrary Player instance Arbitrary Player

deriving instance Generic LineIndex
deriving via GenericArbitrary LineIndex instance Arbitrary LineIndex

deriving instance Generic Board
deriving via GenericArbitrary Board instance Arbitrary Board

main :: IO ()
main = hspec $ do

  it "exercise 1" $ property $ conjoin
    [ practice1 =/= practice2
    , practice3 =/= practice4
    , practice5 === practice5
    ]

  it "exercise 2" $ property $
    forAll arbitrary $ \(i, p, b) ->
      indexBoard (i, updateBoard (i, p, b)) === Just p

  it "exercise 3" $ property $ conjoin
    [ forAll arbitrary $ \b ->
        filterEmptyIndices ([], b) === []
    , forAll arbitrary $ \(NonEmpty is, b) ->
        let is' = filterEmptyIndices (is, b) in
          conjoin
            [ forAll (elements is) $ \i ->
                case indexBoard (i, b) of
                  Nothing -> elem i is'
                  Just _ -> notElem i is'
            , is' === [] .||. forAll (elements is') (flip elem is)
            ]
    ]

  it "exercise 4" $ property $ conjoin
    [ forAll arbitrary $ \(p, b) ->
        findBestMoveInList ([], p, b) === Nothing
    , forAll arbitrary $ \(NonEmpty is, p, b) ->
        forAll (elements is) $ \i ->
          let bestMove = findBestMoveInList (is, p, b) in
            conjoin
              [ isJust bestMove
              , elem (fst (fromJust bestMove)) is
              , snd (fromJust bestMove) >= rateMoveOutcome (i, p, b)
              ]
    ]
