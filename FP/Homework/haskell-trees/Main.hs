module Main where

data Tree a
  = EmptyTree
  | Node
      { value :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show, Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show, Read)

values :: Strategy -> Tree a -> [a]
values _ EmptyTree = []
values Inorder (Node v l r) = values Inorder l ++ (v : values Inorder r)
values Postorder (Node v l r) = values Postorder l ++ values Postorder r ++ [v]
values Preorder (Node v l r) = (v : values Preorder l) ++ values Preorder r

main = do
  putStrLn "This is Main!"