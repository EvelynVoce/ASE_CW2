{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit
import Lib

main :: IO ()
main = do
  results <- runTestTT lookupTests
  insert_results <- runTestTT insertTests
  remove_restults <- runTestTT removeTests
  removeIf_restults <- runTestTT removeIfTests
  return ()


-- Hard Coded BST implementation (Really need to change this at some point)
bst_constructor :: BST String
bst_constructor =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" (Node 2 "ChildAlex" Leaf Leaf) Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))


-- bst_constructor2 :: BST String -> String
-- bst_constructor2 =
--   Node "AB" "Eve"
--     (Node "BB" "Ryder" (Node "CB" "Alex" Leaf Leaf) (Node "DB" "Dom" Leaf Leaf) )
--     (Node "EB" "Harry" (Node "FB" "Neil" Leaf Leaf) (Node "GB" "Random Name" Leaf Leaf))


lookupTests :: Test
lookupTests = TestList [
  TestCase (assertEqual "Lookup Test" (Just "Alex") (bst_lookup 5 bst_constructor)),
  TestCase (assertEqual "Root Node" (Just "Eve") (bst_lookup 20 bst_constructor)),
  TestCase (assertEqual "Not in Tree" Nothing (bst_lookup 33 bst_constructor))
  ]


insertTests :: Test
insertTests = TestList [
  insertTest1,
  insertTest2,
  insertExistingKey]

insertTest1 :: Test
insertTest1 =
  let altered_bst = insert 50 "Testing" bst_constructor in
  TestCase (assertEqual "New node right sub-tree" (Just "Testing") (bst_lookup 50 altered_bst))

insertTest2 :: Test
insertTest2 =
  let altered_bst = insert 3 "SmallestKey" bst_constructor in
  TestCase (assertEqual "New node left sub-tree" (Just "SmallestKey") (bst_lookup 3 altered_bst))

insertExistingKey :: Test
insertExistingKey =
  let altered_bst = insert 15 "New Dom" bst_constructor in
  TestCase (assertEqual "Replace existing node" (Just "New Dom") (bst_lookup 15 altered_bst))



removeTests :: Test
removeTests = TestList [
  removeTest1,
  removeTest2,
  removeTest3,
  removeTest4,
  removeTest5,
  removeTest6
  ]

removeTest1 :: Test
removeTest1 =
  let altered_bst = delete 2 bst_constructor in
  TestCase (assertEqual "Remove node with no children" Nothing (bst_lookup 2 altered_bst))

removeTest2 :: Test
removeTest2 =
  let altered_bst = delete 5 bst_constructor in
  TestCase (assertEqual "Remove node with only left child" (Just "ChildAlex") (bst_lookup 2 altered_bst))

removeTest3 :: Test
removeTest3 =
  let altered_bst = delete 15 bst_constructor in
  TestCase (assertEqual "Remove node with only right child" (Just "ChildDom") (bst_lookup 17 altered_bst))

removeTest4 :: Test
removeTest4 =
  let altered_bst = delete 31 bst_constructor in
  TestCase (assertEqual "Remove node with two children, check node deleted" Nothing (bst_lookup 30 altered_bst))

removeTest5 :: Test
removeTest5 =
  let altered_bst = delete 31 bst_constructor in
  TestCase (assertEqual "Remove node with two children, check left child" (Just "Neil") (bst_lookup 25 altered_bst))

removeTest6 :: Test
removeTest6 =
  let altered_bst = delete 31 bst_constructor in
  TestCase (assertEqual "Remove node with two children, check right child" (Just "Random Name") (bst_lookup 35 altered_bst))

print_tree :: IO()
print_tree =
  inorder_display bst_constructor

isEven :: Int -> Bool
isEven n =
  if n `mod` 2 == 0
    then True
    else False

removeIfTests :: Test
removeIfTests = TestList [
  TestCase (assertEqual "Begone even scum" (delete 2 (delete 10 (delete 20 bst_constructor))) (deleteIf isEven bst_constructor))
  ]

print_tree2 :: IO()
print_tree2 =
  let altered_bst = deleteIf isEven bst_constructor in
  inorder_display altered_bst


print_tree3 :: IO()
print_tree3 =
  let altered_bst = delete 31 bst_constructor in
  inorder_display altered_bst


-- print_tree4 :: IO()
-- print_tree4 =
--   bstToList bst_constructor


-- removeIfTest1 :: Test
-- removeIfTest1 = 
--   let altered_bst = removeIf 