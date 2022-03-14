{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib where
import Control.Exception (BlockedIndefinitelyOnMVar)

data BST item = InternalNode Int item (BST item) (BST item) | Leaf

bst_lookup :: Int -> BST item -> Maybe item
bst_lookup soughtKey Leaf = Nothing
bst_lookup soughtKey (InternalNode key item leftChild rightChild)
  -- | soughtKey == Leaf = Leaf -- Is this needed? 
  | soughtKey > key = bst_lookup soughtKey rightChild
  | soughtKey < key = bst_lookup soughtKey leftChild
  | otherwise = Just item -- If keys match
