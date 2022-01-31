module Division where

import Data.Bool
import Data.Int
import Data.Ord
import GHC.Num

intDiv :: (Int, Int) -> Int
intDiv (x, y) =
    if x < y then
        0
    else
        1 + intDiv (x - y, y)