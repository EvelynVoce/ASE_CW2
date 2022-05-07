module BST where
-- import Control.Exception (BlockedIndefinitelyOnMVar)

data BST key item =
  Node key item (BST key item) (BST key item)
  | Leaf
  deriving (Show, Eq)


-- Creates a new BST
create_bst :: BST key item
create_bst = Leaf


-- Returns the number of nodes in a tree
size :: BST key item -> Int
size Leaf = 0
size (Node key item leftChild rightChild) = 1 + size leftChild + size rightChild


-- Searches a tree for a node with a given key
bst_lookup :: (Ord key) => key -> BST key item -> Maybe item
bst_lookup soughtKey Leaf = Nothing
bst_lookup soughtKey (Node key item leftChild rightChild)
  | soughtKey > key = bst_lookup soughtKey rightChild
  | soughtKey < key = bst_lookup soughtKey leftChild
  | otherwise = Just item -- If keys match


-- Inserts a node into a given tree
insert :: (Ord key) => key -> item -> BST key item -> BST key item
insert insertKey insertValue Leaf = Node insertKey insertValue Leaf Leaf
insert insertKey insertValue (Node key value leftChild rightChild) 
  | key == insertKey = Node key insertValue leftChild rightChild
  | key < insertKey = Node key value leftChild (insert insertKey insertValue rightChild)
  | key > insertKey = Node key value (insert insertKey insertValue leftChild) rightChild


-- Finds the node to be removed from a given tree
remove :: (Ord key) => key -> BST key item -> BST key item
remove remove_key Leaf = Leaf
remove remove_key (Node key item leftChild rightChild)  
  | remove_key == key = remove_node (Node key item leftChild rightChild)
  | remove_key < key = Node key item (remove remove_key leftChild) rightChild
  | remove_key > key = Node key item leftChild (remove remove_key rightChild)


-- Removes the node from a given tree
remove_node :: (Ord key) => BST key item -> BST key item
remove_node (Node key item Leaf Leaf) = Leaf
remove_node (Node key item Leaf rightChild) = rightChild
remove_node (Node key item leftChild Leaf) = leftChild
remove_node (Node key item leftChild rightChild) = (Node key2 item2 leftChild (remove key2 rightChild))
  where (key2, item2) = find_minimum_node rightChild


-- Finds the minimum node in a given tree
find_minimum_node :: (Ord key) => BST key item -> (key, item)
find_minimum_node (Node key item Leaf _) = (key, item)
find_minimum_node (Node key item leftChild _) = find_minimum_node leftChild


-- Removes multiple nodes from a given tree provided the node's key matches a given condition
removeIf :: (Ord key) => (key -> Bool) -> BST key item -> BST key item
removeIf condition Leaf = Leaf
removeIf condition (Node key item leftChild rightChild) =
  if condition key
    then removeIf condition (remove key (Node key item leftChild rightChild))
    else Node key item (removeIf condition leftChild) (removeIf condition rightChild)


-- removeIf :: (Ord key) => (key -> Bool) -> BST key item -> BST ley item
-- removeIf condition Leaf = Leaf
-- removeIf condition (Node key item leftChild rightChild) =
--   removeIf condition leftChild
--   if condition key then remove key (Node key item leftChild rightChild)
--   removeIf condition rightChild


-- Displays keys and items of all nodes in a given tree in order
inorder_display :: (Ord key, Show key, Show item) => BST key item -> IO()
inorder_display Leaf = return ()
inorder_display (Node key item leftChild rightChild) = do
  inorder_display leftChild
  putStrLn (show key ++ " " ++ show item)
  inorder_display rightChild


-- Returns a list of all key, value pairs in order
displayList :: BST key item -> [(key, item)]
displayList Leaf = []
displayList (Node key value Leaf Leaf) = [(key, value)] 
displayList (Node key value leftChild rightChild) = 
  displayList leftChild
  ++ [(key, value)] -- concatenate current node to the existing list
  ++ displayList rightChild -- concatenate the right node to the existing list


-- Extracts the keys from the list returned from DisplayList
get_keys_from_list :: [(key, item)] -> [key]
get_keys_from_list list = 
  map fst list