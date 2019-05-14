module Com.NoSyn.Environment.FunctionEnvironment where

import Data.Map
import Data.Map.Ordered
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable

emptyFunctionEnvironment = Data.Map.empty
defaultFunctionEnvironment = Data.Map.empty

data FunctionOverload = FO {
    returnType :: Ident,
    parameters :: (OMap Ident Variable),
    parentModule :: Maybe String
} deriving Show

type FunctionEnvironment = Map Ident [FunctionOverload]
