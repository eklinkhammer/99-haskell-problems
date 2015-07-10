module Second_Ten
( 
      encode_modified   -- Problem 11
    , decode_run_length -- Problem 12
    , encode_direct     -- Problem 13
    , dupli             -- Problem 14
    , repli             -- Problem 15
    , dropN             -- Problem 16
) where

import First_Ten

data Item a = Single a | Multiple Int a
  deriving (Show)

-- Problem 11
-- Run length encoding, where single items are marked separately
encode_modified :: (Eq a) => [a] -> [Item a]
encode_modified list = map process (encode list)
    where
        process (1,x) = Single x
        process (n,x) = Multiple n x

-- Problem 12
-- Decode the list created in problem 12 (encode_modified) to a regular list
decode_run_length :: (Eq a) => [Item a] -> [a]
decode_run_length [] = []
decode_run_length (x:xs) = (decode x) ++ (decode_run_length xs)
  where
    decode (Single e) = [e]
    decode (Multiple n e) = take n (repeat e)

-- Problem 13
-- Same as problem 11 (run length encoding), but don't create sub lists
encode_direct :: (Eq a) => [a] -> [Item a]
encode_direct [] = []
encode_direct (x:xs) = helper 1 x xs
    where
        helper n y [] = [makeItem (n, y)]
        helper n a (y:ys) | a == y = helper (n+1) a ys 
                          | otherwise = makeItem (n,a) : encode_direct (y:ys)

makeItem :: (Int, a) -> Item a
makeItem (1,x) = Single x
makeItem (n,x) = Multiple n x

-- Problem 14
-- Duplicate each item in a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = helpAdd 2 x (dupli xs)
    where
        helpAdd 1 y [] = [y]
        helpAdd 0 y ys = ys
        helpAdd n y ys = helpAdd (n-1) y (y:ys)

-- Problem 15
-- Duplicate each item in a list a set number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli list 1 = list
repli (x:xs) n = repFirst n x (repli xs n)
    where
        repFirst 1 y [] = [y]
        repFirst 0 y ys = ys
        repFirst n y ys = repFirst (n-1) y (y:ys)

dupli' :: [a] -> [a]
dupli' list = repli list 2

-- Problem 16
-- Drop the Nth element from the list
dropN :: Int -> [a] -> [a]
dropN 1 _ = []
dropN n list = 



