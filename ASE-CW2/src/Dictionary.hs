module Dictionary (create_dictionary, dict_lookup, dict_insert, dict_remove, dict_removeIf, displayList, dict_inorder_display, dict_size) where
import BST

create_dictionary = BST.create_bst
dict_lookup key dictionary = BST.bst_lookup key dictionary
dict_insert key item dictionary = BST.insert key item dictionary
dict_remove key dictionary = BST.delete key dictionary
dict_removeIf condition dictionary = BST.removeIf condition dictionary
displayList dictionary = BST.bstToList dictionary
dict_inorder_display dictionary = BST.inorder_display dictionary
dict_size dictionary = BST.size dictionary