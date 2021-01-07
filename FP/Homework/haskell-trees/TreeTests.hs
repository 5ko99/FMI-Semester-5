module TreeTests where

import Main
  ( Strategy (Inorder, Postorder, Preorder),
    Tree (EmptyTree, Node, left, right, value),
    values,
  )
import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

--Trees
tree1 :: Tree Integer
tree1 =
  Node
    { value = 5,
      left = littleTree1,
      right = littleTree2
    }

littleTree1 :: Tree Integer
littleTree1 =
  Node
    { value = 22,
      left = Node {value = 2, left = EmptyTree, right = EmptyTree},
      right = Node {value = 6, left = EmptyTree, right = EmptyTree}
    }

littleTree2 :: Tree Integer
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

et :: Tree Integer
et = EmptyTree

el :: [Integer]
el = []

tree2 :: Tree Integer
tree2 =
  Node
    { value = 5,
      left =
        Node
          { value = 10,
            left = tree1,
            right = et
          },
      right = Node {value = 22, left = littleTree1, right = littleTree2}
    }

--Tests

treeTests :: Test
treeTests =
  TestList
    [ "tree1-Inorder" ~: values Inorder tree1 ~?= [2, 22, 6, 5, 1, 111, 3],
      "tree1-PreOrder" ~: values Preorder tree1 ~?= [5, 22, 2, 6, 1, 3, 111],
      "tree1-PostOrder" ~: values Postorder tree1 ~?= [2, 6, 22, 111, 3, 1, 5],
      "empty-tree" ~: values Inorder et ~?= el,
      "singleton" ~: values Inorder Node {value = 2, left = EmptyTree, right = EmptyTree} ~?= [2],
      "littleTree-Inorder" ~: values Inorder littleTree1 ~?= [2, 22, 6],
      "littleTree-Preorder" ~: values Preorder littleTree1 ~?= [22, 2, 6],
      "littleTree-Postorder" ~: values Postorder littleTree1 ~?= [2, 6, 22],
      "tree2-Inorder" ~: values Inorder tree2 ~?= [2, 22, 6, 5, 1, 111, 3, 10, 5, 2, 22, 6, 22, 1, 111, 3],
      "tree2-Preorder" ~: values Preorder tree2 ~?= [5, 10, 5, 22, 2, 6, 1, 3, 111, 22, 22, 2, 6, 1, 3, 111],
      "tree2-Postorder" ~: values Postorder tree2 ~?= [2, 6, 22, 111, 3, 1, 5, 10, 2, 6, 22, 111, 3, 1, 22, 5],
      "little-tree-Postorder"
        ~: values
          Postorder
          Node
            { value = 1,
              left = EmptyTree,
              right = Node {value = 2, left = EmptyTree, right = EmptyTree}
            }
          ~?= [2, 1]
    ]

main = do
  runTestTT treeTests