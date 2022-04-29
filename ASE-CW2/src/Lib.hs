{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib where
import Control.Exception (BlockedIndefinitelyOnMVar)
   

data BST item =
  Node Int item (BST item) (BST item)
  | Leaf
  deriving (Show) -- Just displays the tree (useful for visual feedback)


bst_lookup :: Int -> BST item -> Maybe item
bst_lookup soughtKey Leaf = Nothing
bst_lookup soughtKey (Node key item leftChild rightChild)
  | soughtKey > key = bst_lookup soughtKey rightChild
  | soughtKey < key = bst_lookup soughtKey leftChild
  | otherwise = Just item -- If keys match


insert :: Int -> item -> BST item -> BST item
insert insertKey value Leaf = Node insertKey value Leaf Leaf
insert insertKey value (Node key item leftChild rightChild) 
	| key == insertKey = Node key value leftChild rightChild
	| key < insertKey = Node key value leftChild (insert insertKey value rightChild)
	| key > insertKey = Node key value (insert insertKey value leftChild) rightChild


delete :: Int -> BST item -> BST item
-- delete Nil value Leaf = Nil
delete delete_key (Node key item leftChild rightChild)  
	| delete_key == key = delete_node (Node key item leftChild rightChild)
	| delete_key < key = Node key item (delete delete_key leftChild) rightChild
	| delete_key > key = Node key item leftChild (delete delete_key rightChild)


delete_node :: BST item -> BST item
delete_node (Node key item Leaf Leaf) = Leaf
delete_node (Node key item Leaf rightChild) = rightChild
delete_node (Node key item leftChild Leaf) = leftChild
delete_node (Node key item leftChild rightChild) = (Node key2 item leftChild rightChild)
	where 
		key2 = find_minimum_node rightChild


find_minimum_node :: BST item -> Int
find_minimum_node (Node key item Leaf _) = key
find_minimum_node (Node key item leftChild _) = find_minimum_node leftChild