module Lists where

import ListRefPrelude


data ListInt where
    Empty :: ListInt
    NonEmpty :: (Int, ListInt) -> ListInt


myFirstList :: ListInt
myFirstList =
    NonEmpty (1, NonEmpty (2, NonEmpty (3, Empty)))

realList :: [Int]
realList = [1, 2, 3]

sumRealList :: [Int] -> Int
sumRealList [] = 0
sumRealList (x : xs) = x + sumRealList xs

explicitList :: [Int]
explicitList = 1 : (2 : (3 : []))

sumListInt :: ListInt -> Int
sumListInt xs = 
    case xs of
        Empty -> 0
        NonEmpty (y, ys) -> y + sumListInt ys

listIntToString :: ListInt -> String
listIntToString Empty = ""
listIntToString (NonEmpty (x, xs)) =
    show x ++ "," ++ listIntToString xs

realListToString :: [Int] -> String
realListToString [] = ""
realListToString (x : xs) =
    show x ++ "," ++ realListToString xs

instance Show ListInt where show = listIntToString

pairWiseSum :: (ListInt, ListInt) -> ListInt
pairWiseSum (Empty, _) = Empty
pairWiseSum (_, Empty) = Empty
pairWiseSum (NonEmpty (x, xs), NonEmpty (y, ys)) =
    NonEmpty (x+y, pairWiseSum(xs, ys))

realPairWiseSum :: ([Int], [Int]) -> [Int]
realPairWiseSum ([], _) = []
realPairWiseSum (_, []) = []
realPairWiseSum (x : xs, y : ys) =
    (x+y) : realPairWiseSum(xs, ys)

myIndexLookup :: ([Bool], Int) -> Maybe Bool
myIndexLookup ([], _) = Nothing
myIndexLookup (x : xs, i) = 
    if i < 0 then
        Nothing
    else if i == 0 then
        Just x
    else
        myIndexLookup (xs, i-1)



