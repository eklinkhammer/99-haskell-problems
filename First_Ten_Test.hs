import Test.HUnit
import First_Ten

import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit.Base  ((~?=), Test(TestCase, TestList))
import Test.HUnit.Text (runTestTT)
import Test.HUnit.Tools (assertRaises)


-- Unit Tests for myLast - Problem 1
-- Note that I don't know how to check for errors or exceptions
test01_1 = TestCase (assertEqual "myLast 2+ elements" (myLast ['a','b','c']) 'c')
test01_2 = TestCase (assertEqual "myLast 1 element" (myLast [1]) 1)

tests01 = TestList [TestLabel "Test01_1" test01_1, 
                    TestLabel "Test01_2" test01_2]

-- Unit Tests for myButLast - Problem 2
-- Again, I don't have testing for raising error
test02_1 = TestCase (assertEqual "myButLast 3+ elements" (myButLast ['a'..'z']) 'y')
test02_2 = TestCase (assertEqual "myButLast 2 elements" (myButLast [1,2]) 1)

tests02 = TestList [TestLabel "Test02_1" test02_1,
                    TestLabel "Test02_2" test02_2]

-- Unit Tests for elementAt - Problem 3
test03_1 = TestCase (assertEqual "elementAt 1 element" (elementAt [1] 1) 1)
test03_2 = TestCase (assertEqual "elementAt 2 element" (elementAt [1,2] 2) 2)
test03_3 = TestCase (assertEqual "elementAt 2 element" (elementAt [1..10] 4) 4)

tests03 = TestList [  TestLabel "Test03_1" test03_1
                    , TestLabel "Test03_2" test03_2
                    , TestLabel "Test03_3" test03_3]

-- Unit Tests for myLength - Problem 4
test04_1 = TestCase (assertEqual "myLength 0 elements" (myLength []) 0)
test04_2 = TestCase (assertEqual "myLength 1 element"  (myLength [1]) 1)
test04_3 = TestCase (assertEqual "myLength 10 elements"(myLength [1..10]) 10)

tests04 = TestList [  TestLabel "Test04_1" test04_1
                    , TestLabel "Test04_2" test04_2
                    , TestLabel "Test04_3" test04_3]

-- Unit Tests for myReverse - Problem 5
test05_1 = TestCase (assertEqual "myReverse one element" (myReverse [1]) (reverse [1]))
test05_2 = TestCase (assertEqual "myReverse many elements" (myReverse [1..10]) (reverse [1..10]))

tests05 = TestList [TestLabel "Test05_1" test05_1,
                    TestLabel "Test05_2" test05_2]

-- Unit Tests for isPalindrome - Problem 6
test06_1 = TestCase (assertEqual "isPalindrome not Palindrome" (isPalindrome [1,2,3]) False)
test06_2 = TestCase (assertEqual "isPalindrome is Palindrome single center"  (isPalindrome [1,2,3,2,1]) True)
test06_3 = TestCase (assertEqual "isPalindrome is Palindrome duplicate center"  (isPalindrome [1,2,3,3,2,1]) True)

tests06 = TestList [  TestLabel "Test06_1" test06_1
                    , TestLabel "Test06_2" test06_2
                    , TestLabel "Test06_3" test06_3]

---- Unit Tests for flatten - Problem 7
--test07_1 = TestCase (assertEqual "flatten empty list" (flatten (List [])) [])
--test07_2 = TestCase (assertEqual "flatten single element"  (flatten (Elem 5)) [5])
--test07_3 = TestCase (assertEqual "flatten list "(flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) [1,2,3,4,5])

--tests07 = TestList [  TestLabel "Test07_1" test07_1]
--                    , TestLabel "Test07_2" test07_2
--                    , TestLabel "Test07_3" test07_3]
-- Unit Tests for compress - Problem 8
--test08_1 = TestCase (assertEqual "compress empty" (compress []) [])
test08_1 = TestCase (assertEqual "compress single element" (compress [1]) [1])
test08_2 = TestCase (assertEqual "compress to single element" (compress [1,1]) [1])
test08_3 = TestCase (assertEqual "compress only consecutive" (compress [1,1,1,2,1,1]) [1,2,1])
test08_4 = TestCase (assertEqual "compress string" (compress "aaaabccaadeeee") "abcade")

tests08 = TestList [  TestLabel "Test08_1" test08_1
                    , TestLabel "Test08_2" test08_2
                    , TestLabel "Test08_3" test08_3
                    , TestLabel "Test08_4" test08_4]

-- Unit Tests for pack - Problem 9
test09_1 = TestCase (assertEqual "pack single element" (pack [1]) [[1]])
test09_2 = TestCase (assertEqual "pack to single sublist" (pack [1,1]) [[1,1]])
test09_3 = TestCase (assertEqual "pack example string" (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']) ["aaaa","b","cc","aa","d","eeee"])

tests09 = TestList [  TestLabel "Test09_1" test09_1
                    , TestLabel "Test09_2" test09_2
                    , TestLabel "Test09_3" test09_3]

-- Unit Tests for encode - Problem 10
test10_1 = TestCase (assertEqual "pack single element" (encode [1]) [(1,1)])
test10_2 = TestCase (assertEqual "pack to single tuple" (encode [1,1]) [(2,1)])
test10_3 = TestCase (assertEqual "pack example string" (encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']) [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])

tests10 = TestList [  TestLabel "Test10_1" test10_1
                    , TestLabel "Test10_2" test10_2
                    , TestLabel "Test10_3" test10_3]


tests_all = TestList [  TestLabel "Tests01" tests01
                      , TestLabel "Tests02" tests02
                      , TestLabel "Tests03" tests03
                      , TestLabel "Tests04" tests04
                      , TestLabel "Tests05" tests05
                      , TestLabel "Tests06" tests06
                      --, TestLabel "Tests07" tests07
                      , TestLabel "Tests08" tests08
                      , TestLabel "Tests09" tests09
                      , TestLabel "Tests10" tests10]

main = runTestTT tests_all