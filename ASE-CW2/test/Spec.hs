{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.HUnit
import Lib
import BST
import Test.QuickCheck
import Data.List(sort)
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = do
  size_results <- runTestTT sizeTests
  lookup_results <- runTestTT lookupTests
  insert_results <- runTestTT insertTests
  remove_restults <- runTestTT removeTests
  removeIf_restults <- runTestTT removeIfTests
  bstToList_results <- runTestTT bstToListTests
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


bst_inserted_26 :: BST Int String
bst_inserted_26 =
  Node 20 "Eve"
    (Node 10 "Ryder" (Node 5 "Alex" Leaf Leaf) (Node 15 "Dom" Leaf (Node 17 "ChildDom" Leaf Leaf)) )
    (Node 31 "Harry" (Node 25 "Neil" Leaf (Node 26 "InsertTest" Leaf Leaf)) (Node 35 "Random Name" Leaf Leaf))

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
  TestCase (assertEqual "Insert empty" (Just "First") (bst_lookup 5 (insert 5 "First" create_bst))),
  TestCase (assertEqual "Full Tree Test" bst_inserted_26 (insert 26 "InsertTest" bst_inserted_26)),
  TestCase (assertEqual "Replace existing node String" (Just "New Dom") (bst_lookup "D" (insert "D" "New Dom" bst_constructor2)))
  ]


removeTests :: Test
removeTests = TestList [
  TestCase (assertEqual "Remove node with no children" bst_removed_with_no_child (delete 2 bst_constructor)),
  TestCase (assertEqual "Remove node with only left child" bst_removed_with_left_child (delete 5 bst_constructor)),
  TestCase (assertEqual "Remove node with only right child" bst_removed_with_right_child (delete 15 bst_constructor)),
  TestCase (assertEqual "Remove node with two children" bst_removed_two_children (delete 31 bst_constructor)),
  TestCase (assertEqual "Remove none existing node" bst_constructor (delete 120 bst_constructor)),
  TestCase (assertEqual "Remove node with two children, polymorphic" bst2_removed_two_children (delete "G" bst_constructor2))
  ]


removeIfTests :: Test
removeIfTests = TestList [
  TestCase (assertEqual "Remove Evens" (delete 2 (delete 10 (delete 20 bst_constructor))) (removeIf isEven bst_constructor)),
  TestCase (assertEqual "Remove Odds" (Node 20 "Eve" (Node 10 "Ryder" (Node 2 "ChildAlex" Leaf Leaf) Leaf) Leaf) (removeIf isOdd bst_constructor))
  ]


listed_bst_constructor :: [(Int, String)]
listed_bst_constructor = [(2,"ChildAlex"),(5,"Alex"),(10,"Ryder"),(15,"Dom"),(17,"ChildDom"),(20,"Eve"),(25,"Neil"),(31,"Harry"),(35,"Random Name")]

listed_bst_constructor2 :: [(String, String)]
listed_bst_constructor2 = [("B","Alex"),("C","Ryder"),("D","Dom"),("E","Eve"),("F","Neil"),("G","Harry"),("H","Random Name")]

bstToListTests :: Test
bstToListTests = TestList [
  TestCase (assertEqual "List Empty Tree" [] (bstToList (delete 5 (insert 5 "test" create_bst)))),
  TestCase (assertEqual "List constructor" listed_bst_constructor (bstToList bst_constructor)),
  TestCase (assertEqual "List constructor polymorphic" listed_bst_constructor2 (bstToList bst_constructor2))
  ]


sizeTests :: Test
sizeTests = TestList [
  TestCase (assertEqual "Size of empty tree" 0 (size create_bst)),
  TestCase (assertEqual "Size of bst_constructor" 9 (size bst_constructor))
  ]


-- Property tests --

---- Heavily adapted from (Simon Thompson. Haskell: The Craft of Functional Programming. Addison-Wesley, 3rd edition, 2011.) ---  

instance (Arbitrary key, Arbitrary item) => Arbitrary (BST key item) where
   arbitrary = sized buildTree -- sized makes recurssion terminate


buildTree n
  | n > 0 = 
    do
      key <- arbitrary -- arbitrary key
      item <- arbitrary -- arbitrary item
      
      -- Recursively add the next layer of nodes (with decreasing size)
      leftChild <- buildTree (subExp) 
      rightChild <- buildTree (subExp)
    
      return (Node key item leftChild rightChild)
  | otherwise = return Leaf -- Bottom of Tree
  where subExp = div n 2

----------------------------------------------------------------------------------

prop_lookup_after_insert :: Int -> String -> BST Int String -> Bool
prop_lookup_after_insert key item tree = Just(item) == (bst_lookup key (insert key item tree))


prop_increases_size_by_one :: Int -> String -> BST Int String -> Bool
prop_increases_size_by_one key item tree =
  ((bst_lookup key tree == Nothing) && size tree + 1 == size altered_bst) || ((bst_lookup key tree /= Nothing) && size tree == size altered_bst)
    where altered_bst = insert key item tree

prop_decreases_size_by_one :: Int -> String -> BST Int String -> Bool
prop_decreases_size_by_one key item tree =
  size tree == size altered_bst || size tree - 1 == size altered_bst
    where altered_bst = delete key (insert key item tree)

increase_decrease_size :: TestTree
increase_decrease_size = testGroup "increase_decrease_size"
  [
    testProperty "increase size by at most one per insert" prop_increases_size_by_one,
    testProperty "decrease size by at most one per delete" prop_decreases_size_by_one
  ]

run_group :: IO ()
run_group = defaultMain increase_decrease_size

prop_displays_keys_in_order :: Int -> String -> Bool
prop_displays_keys_in_order key item = 
  let altered_bst = insert key item bst_constructor in
    get_keys_from_list (bstToList altered_bst) == sort(get_keys_from_list(bstToList altered_bst))
