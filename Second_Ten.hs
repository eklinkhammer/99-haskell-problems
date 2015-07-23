module Second_Ten
( 
      encode_modified   -- Problem 11
    , decode_run_length -- Problem 12
    , encode_direct     -- Problem 13
    , dupli             -- Problem 14
    , repli             -- Problem 15
    , dropEvery         -- Problem 16
    , split             -- Problem 17
    , split'
    , slice             -- Problem 18
    , rotate            -- Problem 19
    , removeAt          -- Problem 20
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
dropEvery :: [a] -> Int -> [a]
dropEvery _ 1 = []
dropEvery list n = dropNHelper n 1 list
    where
        dropNHelper _ _ [] = []
        dropNHelper stride inc (x:xs)
            | stride == inc = dropNHelper stride 1 xs
            | otherwise = x : dropNHelper stride (inc+1) xs

-- Problem 17
-- Split the list into two lists, one of length n and the other 
--  being the rest of the list.
split :: [a]-> Int -> ([a], [a])
split list n = (first, second)
    where
        first = splitHelpFirst 0 n list
        second = splitHelpSecond 0 n list
        splitHelpFirst _ _ [] = []
        splitHelpFirst ptr l (x:xs) 
            | ptr >= l = []
            | otherwise = x : splitHelpFirst (ptr+1) l xs
        splitHelpSecond _ _ [] = []
        splitHelpSecond ptr l (x:xs)
            | ptr >= l = x : splitHelpSecond (ptr+1) l xs
            | otherwise = splitHelpSecond (ptr+1) l xs


-- Problem 17 - More Haskelly implementation
split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split' xs _ = ([], xs)

-- Problem 18
-- Splices a list (returns the sublist between indecies given)
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 b = x : slice xs 1 (b-1)
slice (x:xs) a b = slice xs (a-1) (b-1)

-- Problem 19
-- Rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate (x:xs) n 
    | n >= 0     = rotate (xs ++ [x]) (n-1)
    | otherwise = rotate (x:xs)    (n + length (x:xs))

-- Problem 20
-- Remove the Kth element from a list
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt k list = (Just (list !! (k)), take k list ++ drop (k+1) list)

