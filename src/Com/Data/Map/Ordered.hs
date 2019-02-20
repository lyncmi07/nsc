module Com.Data.Map.Ordered (union) where

import Data.Map.Ordered as OrderMap
import Com.NoSyn.Error.CompilerStatus
import Com.SetTheory.SetSatisfiable

instance (Ord a) => SetSatisfiable (OMap a b) where
    union m n = fromList $ (assocs m) ++ (assocs n)
    difference = (\\)
    size = OrderMap.size
