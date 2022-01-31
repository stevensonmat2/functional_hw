-- The purpose of this module is to re-export all of the
-- "automatic imports" that Haskell usually gives us ("the
-- Prelude"), but giving some functions more restrictive types
-- than in the standard Prelude.

-- If you check the type of a function like "length" in the
-- standard Prelude (which you can do by loading this file in
-- the REPL with ":l ListRefPrelude" and asking ":t
-- Prelude.length"), you'll see that it involves a funny thing
-- called "Foldable" and a double arrow ("=>"). Broadly, this
-- means that the "length" function works on lists, but not
-- *only* on lists: it works on any "foldable" structure, and
-- lists are just one data structure that we consider to be
-- "foldable". Near the end of this course, we'll discuss
-- exactly what "foldable" means in this context and why this
-- kind of abstraction is useful in practice.

-- Right now, our task is to *introduce* lists for the first
-- time. For this beginner-level learning process, we *don't*
-- want a bunch of fancy abstraction going on; we want to
-- specifically focus on the versions of these functions that
-- work on lists, instead of the more abstract versions that
-- work on general "foldable" structures. This gets us simpler
-- function types that are easier to think about, and provides
-- us with slightly more beginner-friendly error messages when
-- things go wrong.

-- Here we declare the module for this file and declare that the
-- ListRefPrelude module *re-exports* everything that it
-- *imports* from the Prelude module below. We also re-export
-- the *entire* Data.IORef module just for convenience.
module ListRefPrelude
  ( module Prelude
  , module Data.IORef
  )
  where

-- Here we import *everything* from the Data.IORef module, all
-- of which gets re-exported from this ListRefPrelude module.
import Data.IORef

-- Here we import everything from the Prelude module *except*
-- the listed functions, which are specifically not imported.
-- The ordering of the "hiding" list does not affect anything.
import Prelude hiding
  ( foldr, foldl
  , null, length
  , elem, notElem, find
  , maximum, minimum, maximumBy, minimumBy
  , concat, concatMap
  , sum, product, and, or, any, all
  )

-- Here we import the name "Prelude" itself *without* importing
-- any specific types or functions; this allows us to use (for
-- example) the name "Prelude.foldr" to refer to the Prelude
-- version of the "foldr" function, as opposed to the version of the "foldr" function that we define in this file.
import qualified Prelude

-- These function definitions look kind of silly, since we're
-- generally just saying (for example) "my definition of foldr
-- is the already-existing definition of foldr". Again, the
-- point of these definitions is to give *more restrictive*
-- types to each function: Prelude.foldr works on *any*
-- "foldable" type, but our version of foldr works *only* on
-- lists. In "real-world" code, this restriction would be a bad
-- thing, because it would unnecessarily restrict the use cases
-- for each of these functions; for the beginner learning
-- experience, though, it's helpful to hide the complex details
-- at first.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = Prelude.foldr

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = Prelude.foldl

null :: [a] -> Bool
null = Prelude.null

length :: [a] -> Int
length = Prelude.length

elem :: Eq a => a -> [a] -> Bool
elem = Prelude.elem

notElem :: Eq a => a -> [a] -> Bool
notElem = Prelude.elem

sum :: Num a => [a] -> a
sum = Prelude.sum

product :: Num a => [a] -> a
product = Prelude.product

concat :: [[a]] -> [a]
concat = Prelude.concat

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = Prelude.concatMap

and :: [Bool] -> Bool
and = Prelude.and

or :: [Bool] -> Bool
or = Prelude.or

any :: (a -> Bool) -> [a] -> Bool
any = Prelude.any

all :: (a -> Bool) -> [a] -> Bool
all = Prelude.all
