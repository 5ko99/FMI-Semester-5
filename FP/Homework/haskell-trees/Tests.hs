module Tests where

import Main
  ( Strategy (Inorder, Postorder, Preorder),
    Tree (EmptyTree, Node, left, right, value),
    values,
  )
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

--Trees
tree1 =
  Node
    { value = 5,
      left = littleTree1,
      right = littleTree2
    }

littleTree1 =
  Node
    { value = 22,
      left = Node {value = 2, left = EmptyTree, right = EmptyTree},
      right = Node {value = 6, left = EmptyTree, right = EmptyTree}
    }

littleTree2 =
  Node
    { value = 1,
      left = EmptyTree,
      right =
        Node
          { value = 3,
            left = Node {value = 111, left = EmptyTree, right = EmptyTree},
            right = EmptyTree
          }
    }

--Tests

test1 =
  TestList
    [ "tree1-Inorder" ~: values Inorder tree1 ~?= [2, 22, 6, 5, 1, 111, 3],
      "tree1-PreOrder" ~: values Preorder tree1 ~?= [5, 22, 2, 6, 1, 3, 111],
      "tree1-PostOrder" ~: values Postorder tree1 ~?= [2, 6, 22, 111, 3, 1, 5]
    ]

main = do
  runTestTT test1
