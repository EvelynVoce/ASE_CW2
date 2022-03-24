{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit
import Lib

main :: IO ()
main = do
  results <- runTestTT lookupTests
  print results


-- Hard Coded BST implementation (Really need to change this at some point)
bst_constructor :: BST Int -> String
bst_constructor =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" Leaf Leaf) (Node 15 "Dom" Leaf Leaf) )
    (Node 30 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))
  

-- bst_constructor2 :: BST String -> String
-- bst_constructor2 =
--   Node "AB" "Eve"
--     (Node "BB" "Ryder" (Node "CB" "Alex" Leaf Leaf) (Node "DB" "Dom" Leaf Leaf) )
--     (Node "EB" "Harry" (Node "FB" "Neil" Leaf Leaf) (Node "GB" "Random Name" Leaf Leaf))


lookupTests :: Test
lookupTests = TestList [
  TestCase (assertEqual "Lookup Test" (Just "Alex") (bst_lookup 5 bst_constructor)),
  TestCase (assertEqual "Root Node" (Just "Eve") (bst_lookup 20 bst_constructor)),
  TestCase (assertEqual "Not in Tree" Nothing (bst_lookup 33 bst_constructor)),
  
  TestCase (assertEqual "INSERT TEST" (Just "Alex") (insert 50 "Testing" bst_constructor))
  ]
