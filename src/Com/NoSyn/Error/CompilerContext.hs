module Com.NoSyn.Error.CompilerContext where

import Data.Set
import Data.Set.SetTheory
import Data.Set

data CompilerContext = CC {
        moduleDependencies :: (Set String)
    } deriving (Show, Eq)

instance SetSatisfiable CompilerContext where
    union (CC {moduleDependencies = mda}) (CC {moduleDependencies = mdb}) =
        CC {moduleDependencies = (Data.Set.union mda mdb)}
    difference (CC {moduleDependencies = mda}) (CC {moduleDependencies = mdb}) =
        CC {moduleDependencies = (Data.Set.difference mda mdb)}
