import Test.HUnit
import Third_Ten

import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit.Base  ((~?=), Test(TestCase, TestList))
import Test.HUnit.Text (runTestTT)
import Test.HUnit.Tools (assertRaises)


-- Problem 21 - insertAt
-- Inserts an element into a list at a given position
-- insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt_test_1 = TestCase (assertEqual "First element"  [9,1,2,3] (insertAt 9 [1,2,3] 1))
insertAt_test_2 = TestCase (assertEqual "Middle element" [1,9,2,3] (insertAt 9 [1,2,3] 2))
insertAt_test_3 = TestCase (assertEqual "Last element"   [1,2,9,3] (insertAt 9 [1,2,3] 3))
insertAt_test_4 = TestCase (assertEqual "Append element" [1,2,3,9] (insertAt 9 [1,2,3] 4))


tests_insertAt = TestList [   TestLabel "Test 1" insertAt_test_1
                            , TestLabel "Test 2" insertAt_test_2
                            , TestLabel "Test 3" insertAt_test_3
                            , TestLabel "Test 4" insertAt_test_4]

-- Problem 22 - range
-- Create a list with all numbers, inclusive, between two arguments
-- range 4 9
-- [4,5,6,7,8,9]
range_test_1 = TestCase (assertEqual "1 Number" [1] (range 1 1))
range_test_2 = TestCase (assertEqual "2 Number" [1,2] (range 1 2))
range_test_3 = TestCase (assertEqual "5 Number" [1..5] (range 1 5))

tests_range = TestList [      TestLabel "Test 1" range_test_1
                            , TestLabel "Test 2" range_test_2
                            , TestLabel "Test 3" range_test_3]

-- Problem 28 - lsort
-- Sort a list of lists by the length of the sublists
-- lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
lsort_test_1 = TestCase (assertEqual "Empty list" 0 ( sum $ map length $ lsort [[]]))
lsort_test_2 = TestCase (assertEqual "Single element" ["a"] (lsort ["a"]))
lsort_test_3 = TestCase (assertEqual "Keeps order" ["a","b","c"] (lsort ["a","b","c"]))
lsort_test_4 = TestCase (assertEqual "Sort list" ["a","ab", "abc"] (lsort ["ab", "abc", "a"]))

tests_lsort = TestList [      TestLabel "Test 1" lsort_test_1
                            , TestLabel "Test 2" lsort_test_2
                            , TestLabel "Test 3" lsort_test_3
                            , TestLabel "Test 4" lsort_test_4]

-- Problem 28 - lfsort
-- Sort a list of lists by the frequency of the lengths
-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]
lfsort_test_1 = TestCase (assertEqual "Empty list" 0 ( sum $ map length $ lfsort [[]]))
lfsort_test_2 = TestCase (assertEqual "Single element" ["a"] (lfsort ["a"]))
lfsort_test_3 = TestCase (assertEqual "Keeps order" ["a","b","c"] (lfsort ["a","b","c"]))
lfsort_test_4 = TestCase (assertEqual "Sort list" ["a","b", "ab","cd", "ef"] (lsort ["ab", "cd", "ef","a", "b"]))

tests_lfsort = TestList [      TestLabel "Test 1" lfsort_test_1
                            , TestLabel "Test 2" lfsort_test_2
                            , TestLabel "Test 3" lfsort_test_3
                            , TestLabel "Test 4" lfsort_test_4]

tests_all = TestList [    TestLabel "insertAt" tests_insertAt
                        , TestLabel "range" tests_range
                        , TestLabel "lsort" tests_lsort
                        , TestLabel "lfsort" tests_lfsort]

main = runTestTT tests_all