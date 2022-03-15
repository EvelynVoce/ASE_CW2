{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit
import Lib

main :: IO ()
main = do
  results <- runTestTT allTests
  print results

-- Hard Coded BST implementation (Really need to change this at some point)
bst_constructor :: BST String
bst_constructor =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" Leaf Leaf) (Node 15 "Dom" Leaf Leaf) )
    (Node 30 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "I need more friends" Leaf Leaf))

lookup_test_1 :: Test
lookup_test_1 = TestCase (assertEqual "Lookup Test" (Node 5 "Alex" Leaf Leaf) (bst_lookup 5 bst_constructor))

basic_test :: Test
basic_test = TestCase (assertEqual "Test that 5 is " 5 5)

allTests :: Test
allTests = TestList [basic_test]
