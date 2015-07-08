module First_Ten
(     myLast        -- Problem 1
    , myButLast       -- Problem 2
    , elementAt    -- Problem 3
    , myLength     -- Problem 4
    , myReverse    -- Problem 5
    , isPalindrome -- Problem 6

) where

-- Problem 1
-- Return the last element of a list
myLast :: [a] -> a
myLast []         = error "No last element exists!"
myLast [x]        = x
myLast (x:xs)     = myLast xs

-- Problem 2
-- Return the secodn to last element of a list
myButLast :: [a] -> a
myButLast []        = error "Not second-to-last element in an empty list!"
myButLast [x]       = error "No second-to-last element in single element list!"
myButLast [x,_]     = x
myButLast (x:xs)    = myButLast xs

-- Problem 3
-- Find the kth element of a list    
elementAt :: [a] -> Int -> a
elementAt [] _    = error "Index too large."
elementAt x 1     = head x
elementAt x n
    | n > 1     = elementAt (tail x) (n - 1)
    | otherwise = error "Index cannot be negative."

-- Problem 4
-- Finds the length of the list
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs   

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse xs = rev xs []
    where
        rev [] reversed     = reversed
        rev (x:xs) reversed = rev xs (x:reversed)

-- Problem 6
-- Determines if list is a palindorome (same forwards as backwards)
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = reverse list == list