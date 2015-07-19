module First_Ten
(     myLast       -- Problem 1
    , myButLast    -- Problem 2
    , elementAt    -- Problem 3
    , myLength     -- Problem 4
    , myReverse    -- Problem 5
    , isPalindrome -- Problem 6
    , flatten      -- Problem 7
    , compress     -- Problem 8
    , pack         -- Problem 9
    , encode       -- Problem 10
) where

import Data.List (group)
-- Problem 1
myLast :: [a] -> a
myLast = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast = head . reverse . init

-- Problem 3
-- Find the kth element of a list    
elementAt :: [a] -> Int -> a
elementAt [] _    = error "Index too large."
elementAt x 1     = head x
elementAt x n
    | n > 1     = elementAt (tail x) (n - 1)
    | otherwise = error "Index cannot be negative."

-- Problem 4
myLength :: [a] -> Int
myLength list = foldr (+) 0 $ map (\x -> 1) list 

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6
-- Determines if list is a palindorome (same forwards as backwards)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = reverse list == list

-- Problem 7
-- Flatten a list. Take a list of lists and get a list

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = concatMap flatten a

-- Problem 8
-- Removes duplicate elements
compress :: (Eq a) => [a] -> [a]
compress = map head . group

-- Problem 9
-- Pack repeated consecutive elements into a sublist
pack :: (Eq a) => [a] -> [[a]]
pack = group 

-- Problem 10
-- Run length encoding on a list
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
