module Com.NoSyn.Serialization.FunctionEnvironmentSerialization where

import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Data.Variable
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Data.Map
import Data.List

serializeFunctionEnvironment :: FunctionEnvironment -> String
serializeFunctionEnvironment functionEnvironment =
    concat $ intersperse "\n" $ concat $ Prelude.map serializeNamedFunction namedFunctions
    where
        namedFunctions = toDescList functionEnvironment

serializeNamedFunction a = serializeNamedFunction' a []
serializeNamedFunction' :: (Ident, [(Ident, (OMap Ident Variable))]) -> [String] -> [String]
serializeNamedFunction' (_, []) serializedFunctions = reverse serializedFunctions
serializeNamedFunction' (functionName, ((returnType, parameters):xs)) serializedFunctions =
    serializeNamedFunction' (functionName, xs) ((functionName ++ ":" ++ returnType ++ ":" ++ serializedParameters):serializedFunctions)
    where
        parameterList = Data.Map.Ordered.assocs parameters
        parameterVariables = Prelude.map (\(_, y) -> y) parameterList
        variableSerializer (VConst typ name) = typ ++ ";" ++ name
        variableSerializer (VPointer typ name) = typ ++ "*;" ++ name
        serializedParameters = concat $ intersperse "," $ Prelude.map variableSerializer parameterVariables
