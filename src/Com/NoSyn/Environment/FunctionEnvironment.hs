module Com.NoSyn.Environment.FunctionEnvironment where

import Data.Map
import Data.Map.Ordered
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Data.List
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Error.CompilerStatus
import Data.Set

emptyFunctionEnvironment = Data.Map.empty
defaultFunctionEnvironment = Data.Map.empty

data FunctionOverload = FO {
    returnType :: Ident,
    parameters :: (OMap Ident Variable),
    parentModule :: Maybe String
} deriving Show

functionType FO { returnType = returnType, parameters = parameters } = 
    returnType ++ "_" ++ (concat $ intersperse "_" parameterTypes)
    where
        parameterType (_, x) = getTypeNoCheck x
        parameterTypes = Prelude.map parameterType (Data.Map.Ordered.assocs parameters)

instance Eq FunctionOverload where
    a == b = (functionType a) == (functionType b)
instance Ord FunctionOverload where
    a <= b = (functionType a) <= (functionType b)

type FunctionEnvironment = Map Ident [FunctionOverload]

functionOverloadMerge :: [FunctionOverload] -> [FunctionOverload] -> [FunctionOverload]
functionOverloadMerge m n = Data.Set.toList $ Data.Set.union (Data.Set.fromList m) (Data.Set.fromList n)

unifyFunctionEnvironments :: FunctionEnvironment -> FunctionEnvironment -> FunctionEnvironment
unifyFunctionEnvironments = Data.Map.unionWith functionOverloadMerge 

addFunction :: Ident -> FunctionOverload -> FunctionEnvironment -> CompilerStatus FunctionEnvironment
addFunction functionName functionOverload functionEnvironment = case (Data.Map.lookup functionName functionEnvironment) of
    Just otherOverloads -> let overloadSet = (Data.Set.fromList otherOverloads) in
        if functionOverload `Data.Set.member` overloadSet
        then Error $ "Function overloads cannot be overwritten in this context. For function: " ++ functionName ++ " " ++ (show functionOverload)
        else return $ Data.Map.insert functionName (functionOverload:otherOverloads) functionEnvironment
    Nothing -> return $ Data.Map.insert functionName [functionOverload] functionEnvironment
