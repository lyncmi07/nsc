module Com.Data.Map where

import Data.Set.SetTheory as SetTH
import Data.Map as Map

instance (Ord a) => SetSatisfiable (Map a b) where
    union = Map.union
    difference = Map.difference

instance (Ord a) => SizeableSet (Map a b) where
    size = Map.size

instance (Ord a) => MutuallyExcludable (Map a b) where
    isEmpty a = (SetTH.size a) == 0
