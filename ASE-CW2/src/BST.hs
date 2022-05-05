module BST where
-- import Control.Exception (BlockedIndefinitelyOnMVar)

data BST key item =
  Node key item (BST key item) (BST key item)
  | Leaf
  deriving (Show, Eq) -- Just displays the tree (useful for visual feedback)

create_bst :: BST key item
create_bst = Leaf


size :: BST key item -> Int
size Leaf = 0
size (Node key item leftChild rightChild) = 1 + size leftChild + size rightChild


bst_lookup :: (Ord key) => key -> BST key item -> Maybe item
bst_lookup soughtKey Leaf = Nothing
bst_lookup soughtKey (Node key item leftChild rightChild)
  | soughtKey > key = bst_lookup soughtKey rightChild
  | soughtKey < key = bst_lookup soughtKey leftChild
  | otherwise = Just item -- If keys match


insert :: (Ord key) => key -> item -> BST key item -> BST key item
insert insertKey insertValue Leaf = Node insertKey insertValue Leaf Leaf
insert insertKey insertValue (Node key value leftChild rightChild) 
  | key == insertKey = Node key insertValue leftChild rightChild
  | key < insertKey = Node key value leftChild (insert insertKey insertValue rightChild)
  | key > insertKey = Node key value (insert insertKey insertValue leftChild) rightChild


delete :: (Ord key) => key -> BST key item -> BST key item
delete delete_key Leaf = Leaf
delete delete_key (Node key item leftChild rightChild)  
  | delete_key == key = delete_node (Node key item leftChild rightChild)
  | delete_key < key = Node key item (delete delete_key leftChild) rightChild
  | delete_key > key = Node key item leftChild (delete delete_key rightChild)


delete_node :: (Ord key) => BST key item -> BST key item
delete_node (Node key item Leaf Leaf) = Leaf
delete_node (Node key item Leaf rightChild) = rightChild
delete_node (Node key item leftChild Leaf) = leftChild
delete_node (Node key item leftChild rightChild) = (Node key2 item2 leftChild (delete key2 rightChild))
  where (key2, item2) = find_minimum_node rightChild


find_minimum_node :: (Ord key) => BST key item -> (key, item)
find_minimum_node (Node key item Leaf _) = (key, item)
find_minimum_node (Node key item leftChild _) = find_minimum_node leftChild


deleteIf :: (Ord key) => (key -> Bool) -> BST key item -> BST key item
deleteIf condition Leaf = Leaf
deleteIf condition (Node key item leftChild rightChild) =
  if condition key
    then deleteIf condition (delete key (Node key item leftChild rightChild))
    else Node key item (deleteIf condition leftChild) (deleteIf condition rightChild)


-- deleteIf :: (Ord key) => (key -> Bool) -> BST key item -> BST ley item
-- deleteIf condition Leaf = Leaf
-- deleteIf condition (Node key item leftChild rightChild) =
--   deleteIf condition leftChild
--   if condition key then delete key (Node key item leftChild rightChild)
--   deleteIf condition rightChild


inorder_display :: (Ord key, Show key, Show item) => BST key item -> IO()
inorder_display Leaf = return ()
inorder_display (Node key item leftChild rightChild) = do
  inorder_display leftChild
  putStrLn (show key ++ " " ++ show item)
  inorder_display rightChild


bstToList :: BST key item -> [(key, item)]
bstToList Leaf = []
bstToList (Node key value Leaf Leaf) = [(key, value)] 
bstToList (Node key value leftChild rightChild) = 
  bstToList leftChild
  ++ [(key, value)] -- concatenate current node to the existing list
  ++ bstToList rightChild -- concatenate the right node to the existing list


get_keys_from_list :: [(key, item)] -> [key]
get_keys_from_list list = 
  map fst list