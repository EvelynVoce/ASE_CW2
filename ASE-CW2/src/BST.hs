module BST where

data BST key item =
  Node key item (BST key item) (BST key item)
  | Leaf
  deriving (Show, Eq) -- Just displays the tree (useful for visual feedback)