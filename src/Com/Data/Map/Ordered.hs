module Com.Data.Map.Ordered (union) where

import Data.Map.Ordered as OrderMap
import Com.NoSyn.Error.CompilerStatus
import Data.Set.SetTheory as SetTH

instance (Ord a) => SetSatisfiable (OMap a b) where
    union m n = fromList $ (assocs m) ++ (assocs n)
    difference = (\\)

instance (Ord a) => SizeableSet (OMap a b) where
    size = OrderMap.size

instance (Ord a) => MutuallyExcludable (OMap a b) where
    isEmpty a = (SetTH.size a) == 0
