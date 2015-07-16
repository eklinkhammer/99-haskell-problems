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

tests_all = TestList [TestLabel "Tests01" tests01]

main = runTestTT tests_all