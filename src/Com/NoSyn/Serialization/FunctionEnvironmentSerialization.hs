module Com.NoSyn.Serialization.FunctionEnvironmentSerialization where

import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Data.Variable
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Data.Map
import Data.List
import Data.List.Split
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.Nameable
import Com.NoSyn.Environment.FunctionEnvironment

serializeFunctionEnvironment :: FunctionEnvironment -> String
serializeFunctionEnvironment functionEnvironment =
    concat $ intersperse "\n" $ concat $ Prelude.map serializeNamedFunction $ Prelude.filter nosynFunctionPredicate namedFunctions
    where
        namedFunctions = toDescList functionEnvironment
        backTake x = (reverse.(Prelude.take x).reverse)
        nosynFunctionPredicate (functionName, _)
            | (backTake (length "_function") functionName) == "_function" = True
            | (backTake (length "_operator") functionName) == "_operator" = True
            | (backTake (length "_bracketop") functionName) == "_bracketop" = True
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
        variableSerializer (VVariadic typ name) = typ ++ "...;" ++ name
        serializedParameters = concat $ intersperse "," $ Prelude.map variableSerializer parameterVariables

deserializeFunction :: String -> CompilerStatus (Ident, FunctionOverload)
deserializeFunction serializedFunction 
    | rEmpty /= [] = Error ("Invalid serialization: " ++ serializedFunction) serializedFunction
    | otherwise = do
        parameters <- deserializeParameters serializedParameters
        return (functionName, FO {
            returnType = returnType,
            parameters = parameters,
            parentModule = Just moduleName
            })
    where
        splitString = splitOn ":" serializedFunction
        moduleName:r1 = splitString
        functionName:r2 = r1
        returnType:r3 = r2
        serializedParameters:rEmpty = r3

deserializeParameters :: String -> CompilerStatus (OMap Ident Variable)
deserializeParameters serializedParameters = do
    parameterVariables <- case splitParameters of
        "":[] -> return []
        _ -> sequence $ Prelude.map deserializeParameter splitParameters
    let parameters = Prelude.map (\x -> ((getName x), x)) parameterVariables
    return $ Data.Map.Ordered.fromList parameters
    where
        splitParameters = splitOn "," serializedParameters
        backTake x = reverse.(Prelude.take x).reverse
        backDrop x = reverse.(Prelude.drop x).reverse
        deserializeParameter serializedParameter = 
            let paramType:paramName:empty = splitOn ";" serializedParameter in
                if (empty /= []) then Error ("Invalid serialization: " ++ serializedParameter) (serializedParameters)
                else if (backTake 1 paramType) == "*" then return $ VPointer (backDrop 1 paramType) paramName
                else if (backTake 3 paramType) == "..." then return $ VVariadic (backDrop 3 paramType) paramName
                else return $ VConst paramType paramName

