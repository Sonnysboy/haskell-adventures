import Control.Arrow
import Data.List
import Data.Ord (comparing)

-- let's fucking go im so excited for this

-- RULES:
{-
Along with your solution for each task you must include a com-
ment explaining your solution and how it works. Solutions with-
out an explanatory comment will get a score of zero. Your com-
ment should demonstrate a complete understanding of your solu-
tion. In other words, anything is fair game but you must demon-
strate that your understand how it works. If in doubt, include
more detail.
• Comments do not count towards the length of your solutions.
• Type signatures do not count towards the length of your solutions.
• import statements do not count towards the length of your so-
lutions. You may import any modules included in the Haskell
Platform.
• Whitespace does not count towards the length of your solutions.
So there is no need to shove all your code onto one line and take
all the spaces out. Use space as appropriate, indent nicely, etc., but
otherwise try making your code as short as you can.
cis 194: homework 3 2
• You are welcome to include additional functions beyond the ones
required. That is, you are welcome to break up your solutions into
several functions if you wish (indeed, sometimes this may lead to
a very short solution). Of course, such additional functions will be
counted towards the length of your solution (excluding their type
signatures).
• Your final submission should be named Golf.hs. Your file should
define a module named Golf, that is, at the top of your file you
should have
-}

-- hopscotch

-- skips:: [a] -> [[a]]

takeStep :: Int -> [a] -> [a]
takeStep _ [] = []
takeStep n (x : xs) = x : takeStep n (drop (n - 1) xs)

slice :: Int -> Int -> Int -> [a] -> [a]
slice start stop step = takeStep step . take (stop - start) . drop start

skips :: [a] -> [[a]]
skips [] = []
skips list = reverse (skips' list (length list - 1))

skips' :: [a] -> Int -> [[a]] -- where integer is our current index
skips' l 0 = [l]
skips' l num = slice num (length l) (1 + num) l : skips' l (num - 1)

-- takes slices of the array from [1,1,..n] to [2,2,..n] etc. where the middle number is the step.

{-

A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
-}

-- let's check them like this.
{--

    check [2,3,4]
    check [3,4,1]
    check [4,1,5]

-}
localMaxima :: [Integer] -> [Integer]
localMaxima list
  | 3 > length list = [] -- no maxima in here because there aren't any possibilities
  | hasMax' (take 3 list) = (list !! 1) : localMaxima (drop 1 list)
  | otherwise = localMaxima (drop 1 list)

hasMax' :: [Integer] -> Bool
hasMax' list = maximum list == (list !! 1)

{-
Exercise 3 Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

histogram [1,1,1,5] ==
 *
 *
 *   *
==========
0123456789

THIS EXERCISE TOOK ME 2 FUCKING HOURS
https://gyazo.com/a36255a7c278ff033b3d28f834a77227

-}

histogram :: [Integer] -> String
histogram list = reverse (unlines $ generateRows grouped) ++ "\n=========\n0123456789"
  where
    grouped = groupElements list

-- so this groups them into tuples
groupElements :: [Integer] -> [(Integer, Int)]
groupElements list = sortBy (\(x1, _) (x2, _) -> compare x1 x2) (map (head &&& length) $ group $ sort list)

-- this also removes all the tuples that we're done with
downOne :: [(Integer, Int)] -> [(Integer, Int)]
downOne tuples = filter (\(x1, x2) -> 0 < x2) $ map (\(x1, x2) -> (x1, x2 - 1)) tuples

generateRow :: [Integer] -> String
generateRow keys = foldr (\x acc -> acc ++ if x `elem` keys then "*" else " ") "" [1, 2, 3, 4, 5, 6, 7, 8, 9]

generateRows :: [(Integer, Int)] -> [String]
generateRows [] = [""]
generateRows tuples = generateRow (keys tuples) : generateRows (downOne tuples)
  where
    tupleKeys = keys tuples

keys :: [(Integer, Int)] -> [Integer]
keys = map fst

