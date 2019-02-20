module Com.Data.Map where

import Com.SetTheory.SetSatisfiable
import Data.Map as Map

instance (Ord a) => SetSatisfiable (Map a b) where
    union = Map.union
    difference = Map.difference
    size = Map.size


