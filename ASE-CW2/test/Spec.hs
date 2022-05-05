{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit
import Lib
import Dictionary
import BST
import Test.QuickCheck

main :: IO ()
main = do
  size_results <- runTestTT sizeTests
  lookup_results <- runTestTT lookupTests
  insert_results <- runTestTT insertTests
  remove_restults <- runTestTT removeTests
  removeIf_restults <- runTestTT removeIfTests
  return ()


bst_constructor :: BST Int String
bst_constructor =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" (Node 2 "ChildAlex" Leaf Leaf) Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))


odd_bst :: BST Int String
odd_bst =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" (Node 2 "ChildAlex" Leaf Leaf) Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))


bst_removed_with_no_child :: BST Int String
bst_removed_with_no_child =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" Leaf Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))

bst_removed_with_left_child :: BST Int String
bst_removed_with_left_child =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 2 "ChildAlex" Leaf Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))

bst_removed_with_right_child :: BST Int String
bst_removed_with_right_child =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" (Node 2 "ChildAlex" Leaf Leaf) Leaf) (Node 17 "ChildDom" Leaf Leaf) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf Leaf) (Node 35 "Random Name" Leaf Leaf))

bst_removed_two_children :: BST Int String
bst_removed_two_children =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" (Node 2 "ChildAlex" Leaf Leaf) Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 35 "Random Name" (Node 25 "Neil" Leaf Leaf) Leaf)


bst_constructor2 :: BST String String
bst_constructor2 =
  Node "E" "Eve"
    (Node "C" "Ryder" (Node "B" "Alex" Leaf Leaf) (Node "D" "Dom" Leaf Leaf) )
    (Node "G" "Harry" (Node "F" "Neil" Leaf Leaf) (Node "H" "Random Name" Leaf Leaf))

bst2_removed_two_children :: BST String String
bst2_removed_two_children =
  Node "E" "Eve"
    (Node "C" "Ryder" (Node "B" "Alex" Leaf Leaf) (Node "D" "Dom" Leaf Leaf) )
    (Node "H" "Random Name" (Node "F" "Neil" Leaf Leaf) Leaf)


isEven :: Int -> Bool
isEven n = if n `mod` 2 == 0 then True else False

isOdd :: Int -> Bool
isOdd n = if n `mod` 2 /= 0 then True else False


lookupTests :: Test
lookupTests = TestList [
  TestCase (assertEqual "Insert empty" (Just "First") (bst_lookup 5 (insert 5 "First" create_bst))),
  TestCase (assertEqual "Lookup Test" (Just "Alex") (bst_lookup 5 bst_constructor)),
  TestCase (assertEqual "Root Node" (Just "Eve") (bst_lookup 20 bst_constructor)),
  TestCase (assertEqual "Not in Tree" Nothing (bst_lookup 33 bst_constructor)),
  TestCase (assertEqual "Lookup Test polymorphic" (Just "Alex") (bst_lookup "B" bst_constructor2)),
  TestCase (assertEqual "Root Node polymorphic" (Just "Eve") (bst_lookup "E" bst_constructor2)),
  TestCase (assertEqual "Not in Tree polymophic" Nothing (bst_lookup "Z" bst_constructor2))
  ]


insertTests :: Test
insertTests = TestList [
  TestCase (assertEqual "New node right sub-tree" (Just "Testing") (bst_lookup 50 (insert 50 "Testing" bst_constructor))),
  TestCase (assertEqual "New node left sub-tree" (Just "SmallestKey") (bst_lookup 3 (insert 3 "SmallestKey" bst_constructor))),
  TestCase (assertEqual "Replace existing node" (Just "New Dom") (bst_lookup 15 (insert 15 "New Dom" bst_constructor))),
  TestCase (assertEqual "Replace existing node String" (Just "New Dom") (bst_lookup "D" (insert "D" "New Dom" bst_constructor2)))
  ]


removeTests :: Test
removeTests = TestList [
  TestCase (assertEqual "Remove node with no children" bst_removed_with_no_child (delete 2 bst_constructor)),
  TestCase (assertEqual "Remove node with only left child" bst_removed_with_left_child (delete 5 bst_constructor)),
  TestCase (assertEqual "Remove node with only right child" bst_removed_with_right_child (delete 15 bst_constructor)),
  TestCase (assertEqual "Remove node with two children" bst_removed_two_children (delete 31 bst_constructor)),
  TestCase (assertEqual "Remove node with two children, polymorphic" bst2_removed_two_children (delete "G" bst_constructor2))
  ]

removeIfTests :: Test
removeIfTests = TestList [
  TestCase (assertEqual "Remove Evens" (delete 2 (delete 10 (delete 20 bst_constructor))) (deleteIf isEven bst_constructor)),
  TestCase (assertEqual "Remove Odds" (Node 20 "Eve" (Node 10 "Ryder" (Node 2 "ChildAlex" Leaf Leaf) Leaf) Leaf) (deleteIf isOdd bst_constructor))
  ]


sizeTests :: Test
sizeTests = TestList [
  TestCase (assertEqual "Remove Evens" 0 (size create_bst)),
  TestCase (assertEqual "Remove Evens" 9 (size bst_constructor))
  ]


-- Property tests --
prop_test :: Int -> String -> Bool
prop_test key item = Just(item) == (bst_lookup key (insert key item bst_constructor))

prop_increases_size_by_one :: Int -> String -> Bool
prop_increases_size_by_one key item = (size bst_constructor) + 1 == size altered_bst || size(bst_constructor) == size altered_bst
  where altered_bst = insert key item bst_constructor

prop_decreases_size_by_one :: Int -> String -> Bool
prop_decreases_size_by_one key item = (size bst_constructor) == size altered_bst || size(bst_constructor) - 1 == size altered_bst
  where altered_bst = delete key (insert key item bst_constructor)

-- print_tree :: IO()
-- print_tree =
--   inorder_display bst_constructor

-- print_tree2 :: IO()
-- print_tree2 =
--   let altered_bst = deleteIf isEven bst_constructor in
--   inorder_display altered_bst

-- print_tree3 :: IO()
-- print_tree3 =
--   let altered_bst = delete 31 bst_constructor in
--   inorder_display altered_bst

-- print_tree4 :: IO()
-- print_tree4 =
--   bstToList bst_constructor
