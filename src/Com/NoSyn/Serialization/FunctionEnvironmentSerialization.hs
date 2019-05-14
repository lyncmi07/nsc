module Com.NoSyn.Serialization.FunctionEnvironmentSerialization where

import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Data.Variable
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Data.Map
import Data.List

serializeFunctionEnvironment :: FunctionEnvironment -> String
serializeFunctionEnvironment functionEnvironment =
    concat $ intersperse "\n" $ concat $ Prelude.map serializeNamedFunction $ Prelude.filter nosynFunctionPredicate namedFunctions
    where
        namedFunctions = toDescList functionEnvironment
        backTake x = (reverse.(Prelude.take x).reverse)
        nosynFunctionPredicate (functionName, _)
            | (backTake (length "_function") functionName) == "_function" = True
            | (backTake (length "_operator") functionName) == "_operator" = True
            | otherwise = False

serializeNamedFunction a = serializeNamedFunction' a []
serializeNamedFunction' :: (Ident, [FunctionOverload]) -> [String] -> [String]
serializeNamedFunction' (_, []) serializedFunctions = reverse serializedFunctions
serializeNamedFunction' (functionName, (x:xs)) serializedFunctions =
    serializeNamedFunction' (functionName, xs) ((functionName ++ ":" ++ (returnType x) ++ ":" ++ serializedParameters):serializedFunctions)
    where
        parameterList = Data.Map.Ordered.assocs (parameters x)
        parameterVariables = Prelude.map (\(_, y) -> y) parameterList
        variableSerializer (VConst typ name) = typ ++ ";" ++ name
        variableSerializer (VPointer typ name) = typ ++ "*;" ++ name
        serializedParameters = concat $ intersperse "," $ Prelude.map variableSerializer parameterVariables
