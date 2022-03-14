{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib where
import Control.Exception (BlockedIndefinitelyOnMVar)

data BST item =
  Node Int item (BST item) (BST item)
  | Leaf
  deriving (Show)

bst_constructor :: BST String
bst_constructor =
  Node 20 "Eve"
    (Node 10 "Ryder"
      (Node 5 "Alex" Leaf Leaf)
      (Node 15 "Dom" Leaf Leaf)
    )
    (Node 30 "Harry"
      (Node 25 "Neil" Leaf Leaf)
      (Node 35 "I need more friends" Leaf Leaf)
    )

-- bst_lookup :: Int -> BST item -> Maybe item
-- bst_lookup soughtKey Leaf = Nothing
-- bst_lookup soughtKey (Node key item leftChild rightChild)
--   -- | soughtKey == Leaf = Leaf -- Is this needed? 
--   | soughtKey > key = bst_lookup soughtKey rightChild
--   | soughtKey < key = bst_lookup soughtKey leftChild
--   | otherwise = Just item -- If keys match