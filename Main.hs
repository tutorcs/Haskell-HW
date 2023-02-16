https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module Main where

import Test.HUnit
import Prelude hiding (all, concat, reverse, takeWhile, zip, (++))
import Text.ParserCombinators.ReadP (count)
import Data.Bits (Bits(xor))

todo = error "It is your job to fill in this todo"

studentName :: String
studentName = todo

testName :: Test
testName =
  "testName"
    ~: assertBool "You need to provide a `studentName`" (not (null studentName))

{-
The main "entry point" for this assignment runs the tests for each
homework problem below. You should not edit this definition. Instead,
your goal is to modify the problems below so that all of the tests
pass. Note that the definitions in Haskell modules do not need to come
in any particular order; here, the main function uses the definitions
testStyle, testLists, etc, even though their definitions come much
later in the file.
-}

main :: IO ()
main = do
  runTestTT testName
  runTestTT testStyle
  runTestTT testLists
  runTestTT testHO
  runTestTT testFoldr
  runTestTT testTree
  return ()

--------------------------------------------------------------------------------
-- Problem (Good Style)
--------------------------------------------------------------------------------

testStyle :: Test
testStyle =
  "testStyle"
    ~: TestList [tabc, tarithmetic, treverse, tzip]

{-

All of the following Haskell code does what it is supposed to do
(i.e. the tests pass), but it is difficult to read. Rewrite the
following expressions so that they exactly follow the style guide. Be
careful: the style guide includes quite a few rules, and we've broken
most of them in what follows! (You don't need to rewrite the test
following each part, but you do need to make sure that you don't break
the code as you refactor it!)

NOTE: Do not change the name of any of the top level declarations
below, even if you think that they aren't very good (they aren't). We
will be using automatic testing to ensure that you do not break
anything when you rewrite these functions. On the other hand, local
variables (such as function parameters and those bound by let and
where) can and should be renamed.

-}

-- Part One

abc x y z

tabc :: Test
tabc =
  "abc"
    ~: TestList
      [ abc True False True ~?= True,
        abc True False False ~?= False,
        abc False True True ~?= False
      ]

-- Part Two

arithmetic :: ((Int, Int), Int) -> ((Int, Int), Int) -> (Int, Int, Int)

tarithmetic :: Test
tarithmetic =
  "arithmetic"
    ~: TestList
      [ arithmetic ((1, 2), 3) ((4, 5), 6) ~?= (-3, 6, -3),
        arithmetic ((3, 2), 1) ((4, 5), 6) ~?= (7, -14, 7)
      ]

-- Part Three


treverse :: Test
treverse =
  "reverse"
    ~: TestList
      [ reverse [3, 2, 1] ~?= ([1, 2, 3] :: [Int]),
        reverse [1] ~?= ([1] :: [Int])
      ]

-- Part Four

zip (x:xs) (y:ys) = (x,y) : zip xs ys


tzip :: Test
tzip =
  "zip"
    ~: TestList


--------------------------------------------------------------------------------
-- Problem (List library chops)
--------------------------------------------------------------------------------

{-

Define, debug and test the following functions. Some of these
functions are part of the Haskell standard prelude or standard
libraries like Data.List. Their solutions are readily available
online. You should not google for this code: instead, implement them
yourself.

For each part of this problem, you should replace the testcase for
that part based on the description in the comments. Make sure to test
with multiple inputs using TestList. We will be grading your test
cases as well as the correctness and style of your solutions! HINT:
your testing code should include any tests that we give you in the the
comments!

Do not use any list library functions in this problem. This includes
any function from the Prelude or from Data.List thats take arguments
or returns a result with a list type. Note that (:) and [] are data
constructors for the list type, not functions, so you are free to use
them. Please also avoid list comprehension syntax, as it actually
de-sugars into library functions! This also includes foldr/map/etc.
You'll get a chance to use those further below!

-}

testLists :: Test
testLists =
  "testLists"

-- Part One

-- | The 'minimumMaybe` function computes the mininum value of a
-- nonempty list. If the list is empty, it returns Nothing.
--
-- >>> minumumMaybe []
-- Nothing
-- >>> minumumMaybe [2,1,3]
-- Just 1
minimumMaybe :: [Int] -> Maybe Int

tminimumMaybe :: Test

-- Part Two

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False
startsWith :: String -> String -> Bool

tstartsWith :: Test

-- Part Three

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False


-- Part Four

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose (i.e. the version of transpose from Data.List).
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- WAS [[1,4],[2,5],[3,6]]
-- NOW It is your job to fill in this todo
-- >>> transpose []
-- WAS []
-- NOW It is your job to fill in this todo
-- >>> transpose [[]]
-- WAS []
-- NOW It is your job to fill in this todo
-- >>> transpose [[3,4,5]]
-- WAS [[3],[4],[5]]
-- NOW It is your job to fill in this todo
-- >>> transpose [[1,2],[3,4,5]]
-- WAS [[1,3],[2,4]]
-- WAS (WARNING: this one is tricky!)
-- NOW It is your job to fill in this todo
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [[]] = []
transpose [a:as] = [a] : transpose [as]
transpose [a:as,b:bs] = [a,b] : transpose [as,bs]
transpose _ = []



ttranspose :: Test
ttranspose = "transpose" ~: (assertFailure "testcase for transpose" :: Assertion)

-- Part Five

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5


tcountSub :: Test

--------------------------------------------------------------------------------
-- Problem (Higher-order list operations)
--------------------------------------------------------------------------------

{-

Complete these operations which take higher-order functions as
arguments. (For extra practice, you may try to define these operations
using foldr, but that is not required for this problem.) Otherwise,
you may not use any list library functions for this problem.

-}

testHO :: Test

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []
takeWhile :: (a -> Bool) -> [a] -> [a]



ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: (assertFailure "testcase for takeWhile" :: Assertion)

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3
find :: (a -> Bool) -> [a] -> Maybe a


tfind :: Test
tfind = "find" ~: (assertFailure "testcase for find" :: Assertion)

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False
all :: (a -> Bool) -> [a] -> Bool


tall :: Test
tall = "all" ~: (assertFailure "testcase for all" :: Assertion)

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]


tmap2 :: Test
tmap2 = "map2" ~: (assertFailure "testcase for map2" :: Assertion)

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]


tmapMaybe :: Test

root :: Double -> Maybe Double

--------------------------------------------------------------------------------
-- Problem (map and foldr practice for lists)
--------------------------------------------------------------------------------

{-

Go back to the following functions that you defined above and redefine
them using one of the higher-order functions map, foldr or para (see
below). These are the only list library functions that you should use
on this problem. If you need any additional helper functions you must
define them yourself (and any helper functions should also use map,
foldr or para instead of explicit recursion).

-}

testFoldr :: Test
testFoldr = TestList [tconcat', tstartsWith', tendsWith', ttails, tcountSub']

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]

{-

NOTE: remember you cannot use any list functions from the Prelude or
Data.List for this problem, even for use as a helper
function. Instead, define it yourself.

-}

concat' :: [[a]] -> [a]

tconcat' :: Test
tconcat' = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

-- NOTE: use foldr for this one, but it is tricky! (Hint: the value returned by foldr can itself be a function.)

startsWith' :: String -> String -> Bool
startsWith' = todo

tstartsWith' = "tstartsWith'" ~: (assertFailure "testcase for startsWith'" :: Assertion)

-- INTERLUDE: para

{-

Now consider a variant of foldr called para. In the case of cons,
foldr provides access to the head of the list and the result of the
fold over the tail of the list. The para function should do the same,
but should also provide access to the tail of the list (before it has
been processed).

-}

-- | foldr variant that provides access to each tail of the list
para :: (a -> [a] -> b -> b) -> b -> [a] -> b

-- For example, consider the tails function.

-- | The 'tails' function calculates all suffixes of a give list and returns them
-- in decreasing order of length. For example:
--
-- >>> tails "abc"
-- ["abc", "bc", "c", ""],
tails :: [a] -> [[a]]

{-

It is a natural fit to implement tails using para. See if you can
redefine the function above so that the test cases still pass.

-}

tails' = todo

ttails :: Test
ttails =
  "tails"
    ~: TestList
      [ "tails0" ~: tails' "abc" ~?= ["abc", "bc", "c", ""],
        "tails1" ~: tails' "" ~?= [""],
        "tails2" ~: tails' "a" ~?= ["a", ""]
      ]

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

-- NOTE: use para for this one!

endsWith' :: String -> String -> Bool
endsWith' = todo

tendsWith' :: Test
tendsWith' = "endsWith'" ~: (assertFailure "testcase for endsWith'" :: Assertion)

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2
-- >>> countSub "" "aaac"
-- 5

-- (You may use the para and startsWith' functions in countSub'.)

countSub' :: String -> String -> Int
countSub' = todo

tcountSub' = "countSub'" ~: (assertFailure "testcase for countSub'" :: Assertion)

--------------------------------------------------------------------------------
-- Problem (Tree Processing)
--------------------------------------------------------------------------------

testTree :: Test
testTree =
  TestList
    [ tappendTree,
      tinvertTree,
      ttakeWhileTree,
      tallTree,
      tmap2Tree
    ]

{-

This next problem involves writing some library functions for tree
data structures. The following datatype defines a binary tree, storing
data at each internal node.

-}

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

{- This is the definition of a mappping operation for this data structure: -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

{- And here is a fold-like operations for trees: -}

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

{- Use one of these functions to define the following operations over trees. -}

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
--
-- >>> appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty)
-- Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
--
-- and
--
-- >>> appendTree Empty (Branch 'a' Empty Empty)
-- Branch 'a' Empty Empty

appendTree :: Tree a -> Tree a -> Tree a
appendTree = todo

tappendTree :: Test
tappendTree = "appendTree" ~: (assertFailure "testcase for appendTree" :: Assertion)

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--
-- >>> invertTree (Branch ("a",True) Empty Empty)
-- Branch (True,"a") Empty Empty

invertTree :: Tree (a, b) -> Tree (b, a)
invertTree = todo

tinvertTree :: Test
tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)

-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

-- >>> takeWhileTree (< 3) tree1
-- Branch 1 (Branch 2 Empty Empty) Empty
--
-- >>> takeWhileTree (< 0) tree1
-- Empty

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree = todo

ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: (assertFailure "testcase for takeWhileTree" :: Assertion)

-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
--
-- >>> allTree odd tree1
-- False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree = todo

tallTree :: Test
tallTree = "allTree" ~: (assertFailure "testcase for allTree" :: Assertion)

-- WARNING: This one is a bit tricky!  (Hint: use `foldTree` and remember
--  that the value returned by `foldTree` can itself be a function. If you are
-- stuck on this problem, go back to `startsWith` and make sure you understand
-- how that function can work with a single fold.)

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
--
-- >>> map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
-- Branch 4 Empty Empty

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = todo

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)
