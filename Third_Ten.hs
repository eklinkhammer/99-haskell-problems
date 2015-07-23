module Third_Ten
(     insertAt        -- Problem 21
    , range           -- Problem 22
    , lsort           -- Problem 28
    , lfsort          -- Problem 28, Part 2
) where

-- skipping (for now) all problems related to randomness
import Second_Ten
import Data.List
import Data.Ord

insertAt :: a -> [a] -> Int -> [a]
insertAt a list i = let (f,l) = split' list (i-1) in (f ++ (a:l))

range :: Int -> Int -> [Int]
range i j = [i..j]

lsort :: [[a]] -> [[a]]
lsort = sortBy (\a b -> compare (length a) (length b))

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy (\x y -> length x == length y) . lsort