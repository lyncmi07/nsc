module Com.NoSyn.Error.CompilerContext where

import qualified Data.Set as DataSet
import Data.Set.SetTheory
import Com.NoSyn.Error.NonFatalError

empty = CC { moduleDependencies = DataSet.empty, nonFatalErrors = [] }

data CompilerContext = CC {
        moduleDependencies :: (DataSet.Set String),
        nonFatalErrors :: [NonFatalError]
    } deriving (Show, Eq)

instance SetSatisfiable CompilerContext where
    union (CC {moduleDependencies = mda, nonFatalErrors = nfeA}) (CC {moduleDependencies = mdb, nonFatalErrors = nfeB}) =
        CC {moduleDependencies = (DataSet.union mda mdb), nonFatalErrors = (nfeA ++ nfeB) }
    difference (CC {moduleDependencies = mda}) (CC {moduleDependencies = mdb}) =
        CC {moduleDependencies = (DataSet.difference mda mdb), nonFatalErrors = []}
