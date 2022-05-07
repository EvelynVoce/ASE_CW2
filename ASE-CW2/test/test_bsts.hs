module Test_bsts where
import BST

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

listed_bst_constructor :: [(Int, String)]
listed_bst_constructor = [(2,"ChildAlex"),(5,"Alex"),(10,"Ryder"),(15,"Dom"),(17,"ChildDom"),(20,"Eve"),(25,"Neil"),(31,"Harry"),(35,"Random Name")]

listed_bst_constructor2 :: [(String, String)]
listed_bst_constructor2 = [("B","Alex"),("C","Ryder"),("D","Dom"),("E","Eve"),("F","Neil"),("G","Harry"),("H","Random Name")]

bst_empty :: BST Int String
bst_empty = Leaf