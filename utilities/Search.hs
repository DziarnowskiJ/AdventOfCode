module Search where

import Data.Set (Set)
import qualified Data.Set as Set

-- graph represented as a function from nodes to neighbours
type Graph a = a -> Set a

-- breadth-first search: the set in position i consists of
-- the elements reachable from s in i steps and no fewer.
bfs :: Ord a => Graph a -> a -> [Set a]
bfs f s =
    takeWhile (not . Set.null) $
    map snd $
    iterate (expand2 f) $
    (Set.empty, Set.singleton s)

-- Given a pair of (nodes visited earlier, nodes just visited),
-- expand to nodes reachable from those.
expand2 :: Ord a => Graph a ->
    (Set a, Set a) -> (Set a, Set a)
expand2 f (old, new) =
    (seen, Set.difference (unionAll f new) seen)
  where
    seen = Set.union old new

-- the set of nodes reachable in one step from a set
unionAll :: Ord a => (a -> Set a) -> Set a -> Set a
unionAll f s = Set.unions (map f (Set.elems s))
