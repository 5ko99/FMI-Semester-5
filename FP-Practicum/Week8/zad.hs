import Data.Maybe

mapValues :: (t -> b) -> [(a, t)] -> [(a, b)]
mapValues mapper assoc =
  map (\(key, value) -> (key, mapper value)) assoc

testMap :: [([Char], Integer)]
testMap = mapValues (+ 1) [("k", 1), ("k1", 2), ("k2", 4)]

extendWith :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
extendWith [] assocList2 = assocList2
extendWith (h@(key, _) : t) assocList2 = case lookup key assocList2 of
  Nothing -> extendWith t (h : assocList2)
  _ -> extendWith t assocList2

testExtend :: [([Char], Integer)]
testExtend = extendWith testMap [("k1", 10), ("k10", 20)]

--graphPart
data Graph a = Graph [(a, [a])]

exampleGraph = Graph $ zip [1 ..] [[2], [1, 3, 4], [3], [1, 3], [6], []]

vertices :: Graph a -> [a]
vertices (Graph a) = map fst a

children val (Graph assoc) = fromMaybe [] $ lookup val assoc

hasEdge (v1, v2) (Graph g) =
  v2 `elem` children v1 g

parents:: (Eq a) => a -> Graph a -> [a]
parents vertex Graph {..} = map fst $ filter (\(v, children) -> vertex `elem` children) associations
