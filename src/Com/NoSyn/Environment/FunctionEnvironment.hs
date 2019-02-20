module Com.NoSyn.Environment.FunctionEnvironment where

import Data.Map
import Data.Map.Ordered
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable

emptyFunctionEnvironment = Data.Map.empty
defaultFunctionEnvironment = Data.Map.empty

type FunctionEnvironment = Map Ident [(Ident, (OMap Ident Variable))]
