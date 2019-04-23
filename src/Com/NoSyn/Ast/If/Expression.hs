module Com.NoSyn.Ast.If.Expression where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Data.List
import Data.Set as Set
import Data.Map as Map
import Data.Map.Ordered as OrderMap
import Data.Either
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.If.Constant
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Control.Monad

data Expression =
    EFuncCall Ident [Expression]
    | EConst Constant
    | EIdent Ident
    deriving Show

instance TargetCodeGeneratable Expression where
    generateD = generateExpression
instance Typeable Expression where
    getTypeNoCheck (EConst constant) = getTypeNoCheck constant

parameterSetsFromPossibleFunctions::[(Ident, OMap Ident Variable)] -> Int -> [Set Ident]
parameterSetsFromPossibleFunctions [] numberOfParameters = Prelude.take numberOfParameters $ repeat Set.empty
parameterSetsFromPossibleFunctions possibleFunctions@((_, paramMap):_) numberOfParameters =
    let initialParameterTypeSets = Prelude.take numberOfParameters $ repeat Set.empty in
    parameterSetsFromPossibleFunctions' possibleFunctions initialParameterTypeSets

parameterSetsFromPossibleFunctions'::[(Ident, OMap Ident Variable)] -> [Set Ident] -> [Set Ident]
parameterSetsFromPossibleFunctions' [] finalParameterTypes = finalParameterTypes
parameterSetsFromPossibleFunctions' ((_, overloadParameterTypes):xs) initialParameterTypes =
    let listedOverloadParameterTypes = OrderMap.assocs overloadParameterTypes in
    let updatedParameterTypes = Prelude.map (\((_, newParam), allParams) -> (getTypeNoCheck newParam) `Set.insert` allParams) $ zip listedOverloadParameterTypes initialParameterTypes in
    parameterSetsFromPossibleFunctions' xs updatedParameterTypes

possibleFunctionsFromReturnTypes::ProgramEnvironment -> Set Ident -> Ident -> Int -> CompilerStatus [(Ident, OMap Ident Variable)]
possibleFunctionsFromReturnTypes (_, functionEnvironment, _) possibleReturnTypes funcName noOfParams = do
    allFunctions <- lookupFunction funcName functionEnvironment
    let possibleFunctions = Prelude.filter (\(x,y) -> (x `Set.member` possibleReturnTypes) && ((OrderMap.size y) == noOfParams)) allFunctions in
        if (length possibleFunctions) == 0
            then Error $ "There are no function overloads for '" ++ funcName ++ "' that satisfy the return types " ++ (show possibleReturnTypes)
            else return $ possibleFunctions

lookupFunction :: String -> Map Ident [(Ident, OMap Ident Variable)] -> CompilerStatus [(Ident, OMap Ident Variable)]
lookupFunction funcName funcEnvironment =
    compilerStatusFromMaybe ("There is no function '" ++ funcName ++ "'") $ mplus (funcLookup funcName) (mplus (funcLookup (dropPostfix "_function" funcName)) (funcLookup (dropPostfix "_operator" funcName)))
        where
            funcLookup name = Map.lookup name funcEnvironment

possibleFunctionsFromReturnAndParamTypes::ProgramEnvironment -> Set Ident -> Ident -> [Set Ident] ->  CompilerStatus [(Ident, OMap Ident Variable)]
possibleFunctionsFromReturnAndParamTypes programEnvironment possibleReturnTypes funcName possibleParameterTypes = do
    possibleFunctionsByReturnType <- possibleFunctionsFromReturnTypes programEnvironment possibleReturnTypes funcName (length possibleParameterTypes)
    let filteredPossibleFunctions = Prelude.map (\(x,_) -> x) $ Prelude.filter validFunctionPredicate $ zip possibleFunctionsByReturnType (repeat possibleParameterTypes) in
        if (length filteredPossibleFunctions) == 0
            then Error $ "there are no function overloads for '"
                ++ funcName
                ++ "' that satisfy the return types "
                ++ show possibleReturnTypes
                ++ " and parameter types "
                ++ show possibleParameterTypes
            else return $ filteredPossibleFunctions


validFunctionPredicate::((Ident, OMap Ident Variable), [Set Ident]) -> Bool
validFunctionPredicate ((_, parameterMap), parameterTypeSets) =
    validFunctionPredicate' (OrderMap.assocs parameterMap) parameterTypeSets

validFunctionPredicate'::[(Ident, Variable)] -> [Set Ident] -> Bool
validFunctionPredicate' [] [] = True
validFunctionPredicate' _ [] = False
validFunctionPredicate' [] _ = False
validFunctionPredicate' ((_, (VConst x _)):xs) (y:ys)
    | x `Set.member` y = validFunctionPredicate' xs ys
    | otherwise = False
validFunctionPredicate' ((_, (VPointer x _)):xs) (y:ys)
    | x `Set.member` y = validFunctionPredicate' xs ys
    | otherwise = False

generateExpression::ProgramEnvironment -> Expression -> CompilerStatus String
generateExpression programEnvironment functionCall@(EFuncCall _ _) = do
    generatedExpression <- validateAndGenerateD programEnvironment (Set.singleton "Nothing") functionCall
    either (\_ -> Error "Expression is ambiguous") (\(_,x) -> return x) generatedExpression
generateExpression _ (EConst _) = Error "Constant expressions cannot be used in this context"
generateExpression _ (EIdent _) = Error "Identifier expressions cannot be used in this context"

generateExpressionWithReturnType::ProgramEnvironment -> Ident -> Expression -> CompilerStatus String
generateExpressionWithReturnType programEnvironment returnType expression = do
    generatedExpression <- validateAndGenerateD programEnvironment (Set.singleton returnType) expression
    either (\_ -> Error $ "Expression " ++ (show expression) ++ "is ambiguous") (\(_,x) -> return x) generatedExpression

validateAndGenerateD::ProgramEnvironment -> Set Ident -> Expression -> CompilerStatus (Either (Set Ident) (Ident, String))
validateAndGenerateD programEnvironment returnTypes functionCall@(EFuncCall funcName paramExprs) = do
    possibleFunctions <- possibleFunctionsFromReturnTypes programEnvironment returnTypes funcName (length paramExprs)
    let parameterSets = parameterSetsFromPossibleFunctions possibleFunctions (length paramExprs) in
        validateAndGenerateD' programEnvironment returnTypes parameterSets functionCall
validateAndGenerateD programEnvironment returnTypes (EConst const) = do
    constantType <- getNoSynType programEnvironment const
    generatedConstant <- generateD programEnvironment const
    if constantType `Set.member` returnTypes
        then return $ Right (constantType, generatedConstant)
        else Error $ "Constant '" ++ generatedConstant ++ "' given cannot be used in context '" ++ (show returnTypes) ++ "'"
validateAndGenerateD programEnvironment returnTypes (EIdent varName) = do
    variable <- lookupVariableType programEnvironment varName
    variableType <- getNoSynType programEnvironment variable
    if variableType `Set.member` returnTypes
        then return $ Right (variableType, varName)
        else Error $ "Identifier '" ++ varName ++ "' given cannot be used in context '" ++ (show returnTypes) ++ "'"

validateAndGenerateD'::ProgramEnvironment -> Set Ident -> [Set Ident] -> Expression -> CompilerStatus (Either (Set Ident) (Ident, String))
validateAndGenerateD' programEnvironment possibleReturnTypes possibleParameterTypes functionCall@(EFuncCall funcName paramExpressions) = do
    reducedParameterTypeEithers <- reduceParameterTypes programEnvironment possibleParameterTypes paramExpressions
    possibleFunctions <-
        let reducedParameterTypes = parameterTypesFromEithers reducedParameterTypeEithers in
            possibleFunctionsFromReturnAndParamTypes programEnvironment possibleReturnTypes funcName reducedParameterTypes
    let reducedReturnTypes = Prelude.foldl (\typeSet (nextType, _) -> nextType `Set.insert` typeSet) Set.empty possibleFunctions in
        if (length possibleFunctions) == 1 
            then let funcToGenerate@(funcReturnType,_) = head possibleFunctions in
                let finalReducedParameterTypes = parameterSetsFromPossibleFunctions possibleFunctions (length paramExpressions) in
                reduceParameterTypes programEnvironment finalReducedParameterTypes paramExpressions >>= 
                    (\finalReducedParameterTypeEithers ->
                        generateDFunctionCall programEnvironment funcName funcToGenerate finalReducedParameterTypeEithers >>=
                            (\x -> return $ Right (funcReturnType, x)))
            else let reducedParameterTypes = parameterTypesFromEithers reducedParameterTypeEithers in
                if reductionWasMade possibleReturnTypes reducedReturnTypes possibleParameterTypes reducedParameterTypes
                    then validateAndGenerateD' programEnvironment reducedReturnTypes reducedParameterTypes functionCall 
                    else return $ Left reducedReturnTypes

reduceParameterTypes::ProgramEnvironment -> [Set Ident] -> [Expression] -> CompilerStatus [(Either (Set Ident) (Ident, String))]
reduceParameterTypes programEnvironment possibleParameterTypes parameterExpressions =
    let typesAndExprs = zip possibleParameterTypes parameterExpressions in
    let typeReducer = \(paramTypes, paramExpr) -> validateAndGenerateD programEnvironment paramTypes paramExpr in
        sequence $ Prelude.map typeReducer typesAndExprs

reductionWasMade::Set Ident -> Set Ident -> [Set Ident] -> [Set Ident] -> Bool
reductionWasMade originalReturnTypes updatedReturnTypes originalParameterTypes updatedParameterTypes
    | returnTypesWereReduced || parameterTypesWereReduced = True
    | otherwise = False
    where
        sizeReduced a b = (Set.size a) > (Set.size b)
        returnTypesWereReduced = sizeReduced originalReturnTypes updatedReturnTypes
        parameterTypesWereReduced = 
            Prelude.foldl (||) False $ Prelude.map (\(x,y) -> sizeReduced x y) $ zip originalParameterTypes updatedParameterTypes

parameterTypesFromEithers::[Either (Set Ident) (Ident, String)] -> [Set Ident]
parameterTypesFromEithers parameterTypeEithers =
    Prelude.map (either id (\(x,_) -> Set.singleton x)) parameterTypeEithers

generateDFunctionCall::ProgramEnvironment -> Ident -> (Ident, OMap Ident Variable) -> [Either (Set Ident) (Ident, String)] -> CompilerStatus String
generateDFunctionCall programEnvironment funcName (returnType, paramTypesAndNames) paramTypeEithers = do
    generatedParameters <- sequence $ Prelude.map (either (\_ -> Error "Parameter expression could not be generated") (\(_,x) -> return x)) paramTypeEithers
    let addressWrappedParameters = Prelude.map (\((_,x),y) -> addressWrapper x y) (zip (OrderMap.assocs paramTypesAndNames) generatedParameters) in
        let paramTypes = Prelude.map (\(_, x) -> getTypeNoCheck x) $ OrderMap.assocs paramTypesAndNames in
            generateDFunctionCall' programEnvironment funcName returnType paramTypes addressWrappedParameters
    where
        addressWrapper (VPointer _ _) x = "&(" ++ x ++ ")"
        addressWrapper _ x = x

generateDFunctionCall'::ProgramEnvironment -> Ident -> Ident -> [Ident] -> [String] -> CompilerStatus String
generateDFunctionCall' programEnvironment@(aliasEnvironment, _, _) funcName returnType parameterTypes parameterExpressions =
    let joinedParameterExpressions = concat $ intersperse "," parameterExpressions in
    return $ fullDName ++ "(" ++ joinedParameterExpressions ++ ")"
    where
        fullDName = case functionIsNative programEnvironment funcName of
          (Just postfix) -> dropPostfix postfix funcName
          Nothing -> let dFuncSuffix = returnType ++ (concat $ Prelude.map id parameterTypes) in
                funcName ++ "_" ++ dFuncSuffix

dropPostfix postfix = (reverse.(Prelude.drop $ length postfix).reverse)

functionIsNative:: ProgramEnvironment -> Ident -> Maybe String
functionIsNative (_, funcEnvironment, _) funcName
    | ((dropPostfix "_function" funcName) `Map.member` funcEnvironment) = return "_function"
    | ((dropPostfix "_operator" funcName) `Map.member` funcEnvironment) = return "_operator"
    | otherwise = Nothing
