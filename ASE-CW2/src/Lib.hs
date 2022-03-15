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
  -- | soughtKey == Leaf = Leaf -- Is this needed? 
  | soughtKey > key = bst_lookup soughtKey rightChild
  | soughtKey < key = bst_lookup soughtKey leftChild
  | otherwise = Just item -- If keys match