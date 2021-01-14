import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

isLeaf :: Tree a -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

testTree :: Tree Int
testTree =
  Node
    5
    (Node 6 Empty Empty)
    ( Node
        (-10)
        (Node 2 Empty Empty)
        Empty
    )

prune :: Tree a -> Tree a
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val l r) = Node val (prune l) (prune r)

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix lst1 lst2
  | lst1 `isPrefixOf` lst2 = Just $ drop (length lst1) lst2
  | otherwise = Nothing

stripPrefix' :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix' [] lst2 = Just lst2
stripPrefix' lst1 [] = Nothing
stripPrefix' (x : xs) (y : ys)
  | x == y = stripPrefix' xs ys
  | otherwise = Nothing

safeUncons :: [a] -> Maybe (a, [a])
safeUncons [] = Nothing
safeUncons (x : xs) = Just (x, xs)

rotateLeft, rotateRight :: Tree a -> Tree a
rotateLeft (Node p a (Node q b c)) = Node q (Node p a b) c
rotateLeft t = t
rotateRight (Node q (Node p a b) c) = Node p a (Node q b c)
rotateRight t = t

----------------
type Graph = [[Int]]

testGraph :: Graph
testGraph = [[1, 2], [2, 5], [3], [5], [], [4]]

graphSize :: Graph -> Int
graphSize = length

neighbs :: Int -> Graph -> [Int]
neighbs u g = g !! u

data Color = White | Gray | Black deriving (Eq, Show)

type State = [Color]

update idx val lst = (take idx lst) ++ (val : drop (idx + 1) lst)

dfs :: Graph -> State
dfs g = foldl helper (replicate n White) [0 .. (n - 1)]
  where
    n = graphSize g
    helper :: State -> Int -> State
    helper colors u
      | colors !! u == White = dfsVisit u colors
      | otherwise = colors
    dfsVisit :: Int -> State -> State
    dfsVisit u colors =
      let colors' = foldl helper' (update u Gray colors) (neighbs u g)
       in update u Black colors'
      where
        helper' :: State -> Int -> State
        helper' colors v
          | colors !! v == White = dfsVisit v colors
          | otherwise = colors

main :: IO ()
main = do
  print 2