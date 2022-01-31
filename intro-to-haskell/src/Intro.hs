module Intro where

import Data.Int
import Data.String
import GHC.Float
import GHC.Num
import Text.Show

myNum :: Int
myNum = 1

-- alternate definition
-- data Bool = True | False

data Bool where
    True :: Bool
    False :: Bool

-- input -> ouput
boolToString :: Bool -> String
boolToString bool = 
    case bool of
        True -> "true"
        False -> "false"

instance Show Bool where show = boolToString

not :: Bool -> Bool 
not b =
    case b of
        True -> False
        False -> True

-- alt definition
not2 :: Bool -> Bool
not2 True = False
not2 False = True

-- True returns false, all else return true
-- "_" matches any constructor pattern
not3 :: Bool -> Bool
not3 x =
    case x of
        True -> False
        _ -> True

myTuple :: (Int, Bool)
myTuple = (1, True)

myThruple :: (Int, String, Bool)
myThruple = (2, "hello", False)

and :: (Bool, Bool) -> Bool
and (x, y) =
    case x of
        True ->
            case y of
                True -> True
                False -> False
        False -> 
            case y of
                True -> False
                False -> False

and2 :: (Bool, Bool) -> Bool
and2 (True, True) = True
and2 _ = False

-- intDiv :: (Int, Int) -> Int
-- intDiv (x, y) = undefined

