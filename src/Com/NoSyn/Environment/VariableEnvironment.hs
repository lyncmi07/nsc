module Com.NoSyn.Environment.VariableEnvironment where

import Data.Map
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable

emptyVariableEnvironment = empty
defaultVariableEnvironment = empty

type VariableEnvironment = Map Ident Variable
