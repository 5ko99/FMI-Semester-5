data BinTree a
  = Empty
  | Node a (BinTree a) (BinTree a)
  deriving (Show)

pushBST :: Ord a => a -> BinTree a -> BinTree a
pushBST el Empty = Node el Empty Empty
pushBST el t@(Node root left right)
  | el == root = t
  | el > root = Node root left (pushBST el right)
  | otherwise = Node root (pushBST el left) right

--example
t = Node 2 (Node 1 Empty Empty) Empty

elementBST :: Ord t => t -> BinTree t -> Bool
elementBST _ Empty = False
elementBST el (Node root left right)
  | el == root = True
  | el > root = elementBST el right
  | otherwise = elementBST el left