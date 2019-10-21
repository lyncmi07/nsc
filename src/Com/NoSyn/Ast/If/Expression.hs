module Com.NoSyn.Ast.If.Expression where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Data.List
import Data.Set as Set
import Data.Map as Map
import Data.Map.Ordered as OrderMap
import Data.Either
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionTraits
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.If.Constant
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Control.Monad
import Com.NoSyn.Environment.FunctionEnvironment
import Data.Set

data Expression =
    EFuncCall Ident [SourcePosition Expression]
    | EConst (SourcePosition Constant)
    | EIdent Ident
    deriving Show

instance TargetCodeGeneratable Expression where
    generateD = generateExpression
instance Typeable Expression where
    getTypeNoCheck (EConst constant) = getTypeNoCheck constant
    getAlphaTypeName pg (EConst constant) = getAlphaTypeName pg constant

parameterSetsFromPossibleFunctions::[FunctionOverload] -> [SourcePosition Expression] -> [Set Ident]
parameterSetsFromPossibleFunctions [] parameterExpressions = Prelude.take (length parameterExpressions) $ repeat Set.empty
parameterSetsFromPossibleFunctions possibleFunctions@(FO { parameters = paramMap }:_) parameterExpressions =
    let initialParameterTypeSets = Prelude.take (length parameterExpressions) $ repeat Set.empty in
    parameterSetsFromPossibleFunctions' possibleFunctions initialParameterTypeSets parameterExpressions

parameterSetsFromPossibleFunctions'::[FunctionOverload] -> [Set Ident] -> [SourcePosition Expression] -> [Set Ident]
parameterSetsFromPossibleFunctions' [] finalParameterSets _ = finalParameterSets
parameterSetsFromPossibleFunctions' (functionOverload:xs) currentParameterSets parameterExpressions =
    let overloadParameters = OrderMap.assocs (parameters functionOverload) in
    let updatedParameterSets = addOverloadTypesToTypeSets overloadParameters currentParameterSets (getContents $ sequence parameterExpressions) in
    parameterSetsFromPossibleFunctions' xs updatedParameterSets parameterExpressions
    where
        addOverloadTypesToTypeSets _ [] _ = []
        addOverloadTypesToTypeSets (x@(_, newParam@(VVariadic _ _)):[]) (y:ys) (z:zs) =
            ((getTypeNoCheck newParam) `Set.insert` y):(addOverloadTypesToTypeSets [x] ys zs)
        addOverloadTypesToTypeSets ((_, newParam@(VPointer _ _)):xs) (y:ys) (z:zs) = case z of
            (EIdent _) -> ((getTypeNoCheck newParam) `Set.insert` y):(addOverloadTypesToTypeSets xs ys zs)
            otherwise -> y:(addOverloadTypesToTypeSets xs ys zs)
        addOverloadTypesToTypeSets ((_, newParam):xs) (y:ys) (z:zs) =
            ((getTypeNoCheck newParam) `Set.insert` y):(addOverloadTypesToTypeSets xs ys zs)

possibleFunctionsFromReturnTypes::ProgramEnvironment -> Set Ident -> Ident -> Int -> CompilerStatus [FunctionOverload]
possibleFunctionsFromReturnTypes (PE { functions = functionEnvironment }) possibleReturnTypes funcName noOfParams = do
    allFunctions <- lookupFunction funcName functionEnvironment
    let possibleFunctions = Prelude.filter (\fo -> ((returnType fo) `Set.member` possibleReturnTypes) && (parameterNumberCheckFunction fo noOfParams (OrderMap.size (parameters fo)))) allFunctions in
        if (length possibleFunctions) == 0
            then Error ("There are no function overloads for '" ++ funcName ++ "' that satisfy the return types " ++ (show possibleReturnTypes)) (show functionEnvironment)
            else return $ possibleFunctions
    where
        reverseTake x = (Prelude.take x).reverse
        parameterNumberCheckFunction functionOverload = case (reverseTake 1 $ OrderMap.assocs (parameters functionOverload)) of
            [(_, (VVariadic _ _))] -> (>=)
            otherwise -> (==)


lookupFunction :: String -> Map Ident [FunctionOverload] -> CompilerStatus [FunctionOverload]
lookupFunction funcName funcEnvironment =
    compilerStatusFromMaybe ("There is no function '" ++ funcName ++ "'") $ mplus (funcLookup funcName) (mplus (funcLookup (dropPostfix "_function" funcName)) (funcLookup (dropPostfix "_operator" funcName)))
        where
            funcLookup name = Map.lookup name funcEnvironment

possibleFunctionsFromReturnAndParamTypes::ProgramEnvironment -> Set Ident -> Ident -> [Set Ident] ->  CompilerStatus [FunctionOverload]
possibleFunctionsFromReturnAndParamTypes programEnvironment possibleReturnTypes funcName possibleParameterTypes = do
    possibleFunctionsByReturnType <- possibleFunctionsFromReturnTypes programEnvironment possibleReturnTypes funcName (length possibleParameterTypes)
    let filteredPossibleFunctions = Prelude.map (\(x,_) -> x) $ Prelude.filter (validFunctionPredicate programEnvironment)  $ zip possibleFunctionsByReturnType (repeat possibleParameterTypes) in
        if (length filteredPossibleFunctions) == 0 then 
                Error ("there are no function overloads for '"
                ++ funcName
                ++ "' that satisfy the return types "
                ++ show possibleReturnTypes
                ++ " and parameter types "
                ++ show possibleParameterTypes) (show possibleFunctionsByReturnType)
            else return $ filteredPossibleFunctions


validFunctionPredicate::ProgramEnvironment -> (FunctionOverload, [Set Ident]) -> Bool
validFunctionPredicate pg (FO { parameters = parameterMap }, parameterTypeSets) =
    validFunctionPredicate' pg (OrderMap.assocs parameterMap) parameterTypeSets

validFunctionPredicate'::ProgramEnvironment -> [(Ident, Variable)] -> [Set Ident] -> Bool
validFunctionPredicate' _ [] [] = True
validFunctionPredicate' _ ((_, (VVariadic paramType _)):[]) [] = True
validFunctionPredicate' _ _ [] = False
validFunctionPredicate' _ [] _ = False
validFunctionPredicate' pg ((_, (VConst x _)):xs) (y:ys)
    | (x `Set.member` y) || ((x ++ "PTR") `Set.member` y) = validFunctionPredicate' pg xs ys
    | otherwise = False
validFunctionPredicate' pg ((_, (VPointer x _)):xs) (y:ys)
    | (x `Set.member` y) || ((x ++ "PTR") `Set.member` y) = validFunctionPredicate' pg xs ys
    | otherwise = False
validFunctionPredicate' pg (x@(_, paramVariable@(VVariadic paramType _)):[]) (y:ys)
    | paramType `Set.member` y = validFunctionPredicate' pg [x] ys
    | otherwise = case (getAlphaTypeName (aliases pg) paramVariable) >>= (\x -> return $ x `Set.member` y) of
        Valid _ x -> x && ((length ys) == 0)
        _ -> False

generateExpression::ProgramEnvironment -> SourcePosition Expression -> CompilerStatus String
generateExpression programEnvironment spExpr = case getContents spExpr of
    functionCall@(EFuncCall _ _) -> do
        generatedExpression <- validateAndGenerateD programEnvironment (Set.singleton "Nothing") spExpr
        either (\_ -> PositionedError (getSourcePosition spExpr) ("Expression '" ++ (show functionCall) ++ "' is ambiguous") (show functionCall)) (\(_,x) -> return x) generatedExpression
    EConst a -> PositionedError (getSourcePosition spExpr) "Constant expressions cannot be used in this context" (show a)
    EIdent a -> PositionedError (getSourcePosition spExpr) "Identifier expressions cannot be used in this context" (show a)

generateExpressionWithReturnType::ProgramEnvironment -> Ident -> SourcePosition Expression -> CompilerStatus String
generateExpressionWithReturnType programEnvironment returnType spExpression = do
    generatedExpression <- validateAndGenerateD programEnvironment (Set.singleton returnType) spExpression
    either (\_ -> PositionedError (getSourcePosition spExpression) ("Expression " ++ (show $ getContents spExpression) ++ "is ambiguous") (show $ getContents spExpression)) (\(_,x) -> return x) generatedExpression

generateExpressionForConstParameter::ProgramEnvironment -> String -> SourcePosition Expression -> CompilerStatus String
generateExpressionForConstParameter programEnvironment generatedExpression spExpr = case getContents spExpr of
    EIdent varName -> do
        variable <- lookupVariableType programEnvironment varName
        case variable of
            (VPointer _ _) -> return $ "*" ++ generatedExpression
            (VConst _ _) -> return generatedExpression
            (VVariadic _ _) -> return generatedExpression
    otherwise -> return generatedExpression

validateAndGenerateD::ProgramEnvironment -> Set Ident -> SourcePosition Expression -> CompilerStatus (Either (Set Ident) (Ident, String))
validateAndGenerateD programEnvironment returnTypes spExpr = case getContents spExpr of
    functionCall@(EFuncCall funcName paramExprs) -> do
        unaliasedNonPointerReturnTypes <- sequence [lookupAtomicNoSynType x (aliases programEnvironment) | x <- (Data.Set.toList returnTypes), backTake 3 x /= "PTR"]
        possibleFunctions <- 
            providePositionInfo spExpr $ possibleFunctionsFromReturnTypes programEnvironment (Data.Set.fromList unaliasedNonPointerReturnTypes) funcName (length paramExprs)
        let parameterSets = parameterSetsFromPossibleFunctions possibleFunctions paramExprs in
                validateAndGenerateD' programEnvironment (Data.Set.fromList unaliasedNonPointerReturnTypes) parameterSets spExpr
        where
            backTake x = reverse.(Prelude.take x).reverse
    EConst const -> do
        constantType <- getNoSynType programEnvironment const
        generatedConstant <- generateD programEnvironment const
        if constantType `Set.member` returnTypes
            then return $ Right (constantType, generatedConstant)
            else PositionedError (getSourcePosition spExpr) ("Constant '" ++ generatedConstant ++ "' given cannot be used in context") (show returnTypes)
    EIdent varName -> do
        variable <- lookupVariableType programEnvironment varName
        variableType <- getNoSynType programEnvironment variable
        alphaVariableType <- getAlphaTypeName (aliases programEnvironment) variable
        if variableType `Set.member` returnTypes
            then return $ Right (alphaVariableType, varName)
            else PositionedError (getSourcePosition spExpr) ("Identifier '" ++ varName ++ "' given cannot be used in context") (show returnTypes)

validateAndGenerateD'::ProgramEnvironment -> Set Ident -> [Set Ident] -> SourcePosition Expression -> CompilerStatus (Either (Set Ident) (Ident, String))
validateAndGenerateD' programEnvironment possibleReturnTypes possibleParameterTypes spFunctionCall = case getContents spFunctionCall of
    functionCall@(EFuncCall funcName paramExpressions) -> do
        reducedParameterTypeEithers <- reduceParameterTypes programEnvironment possibleParameterTypes paramExpressions
        possibleFunctions <-
            let reducedParameterTypes = parameterTypesFromEithers reducedParameterTypeEithers in
                providePositionInfo spFunctionCall $ possibleFunctionsFromReturnAndParamTypes programEnvironment possibleReturnTypes funcName reducedParameterTypes
        let reducedReturnTypes = Prelude.foldl (\typeSet (FO { returnType = nextType }) -> nextType `Set.insert` typeSet) Set.empty possibleFunctions in
            let maybeFuncToGenerate = 
                    if (length possibleFunctions) == 1 then 
                        Just $ head possibleFunctions 
                    else let reducedParameterTypes = parameterTypesFromEithers reducedParameterTypeEithers in 
                        reduceOnPointerParameters programEnvironment possibleFunctions reducedParameterTypes in 
                case maybeFuncToGenerate of
                    Just funcToGenerate ->
                        let finalReducedParameterTypes = parameterSetsFromPossibleFunctions possibleFunctions paramExpressions in
                        reduceParameterTypes programEnvironment finalReducedParameterTypes paramExpressions >>= 
                            (\finalReducedParameterTypeEithers ->
                                providePositionInfo spFunctionCall $
                                    generateDFunctionCall programEnvironment funcName funcToGenerate finalReducedParameterTypeEithers paramExpressions >>=
                                        (foundFunctionReturn funcToGenerate))
                    _ -> let reducedParameterTypes = parameterTypesFromEithers reducedParameterTypeEithers in
                        if reductionWasMade possibleReturnTypes reducedReturnTypes possibleParameterTypes reducedParameterTypes then 
                            validateAndGenerateD' programEnvironment reducedReturnTypes reducedParameterTypes spFunctionCall
                        else 
                            return $ Left reducedReturnTypes
    where
        foundFunctionReturn functionOverload compileString = case (parentModule functionOverload) of
            (Just dependency) -> dependencyRequired dependency $ Right ((returnType functionOverload), compileString)
            Nothing -> return $ Right ((returnType functionOverload), compileString)

reduceOnPointerParameters::ProgramEnvironment -> [FunctionOverload] -> [Set Ident] -> (Maybe FunctionOverload)
reduceOnPointerParameters pg functionOverloads parameterTypes =
    let reducedFunctions = reduceOnPointerParameters' pg functionOverloads parameterTypes in
    if (length reducedFunctions) == 1 then
        let (finalFunction:_) = reducedFunctions in Just finalFunction
    else Nothing
        
reduceOnPointerParameters'::ProgramEnvironment -> [FunctionOverload] -> [Set Ident] -> [FunctionOverload]
reduceOnPointerParameters' _ [] _ = []
reduceOnPointerParameters' pg (x:xs) parameterTypes
    | reduceOnPointerParameters'' pg [z | (_, z) <- 
        OrderMap.assocs $ parameters x] (Prelude.map (head.(Set.toList)) parameterTypes) = x:(reduceOnPointerParameters' pg xs parameterTypes)
    | otherwise = reduceOnPointerParameters' pg xs parameterTypes

reduceOnPointerParameters''::ProgramEnvironment -> [Variable] -> [Ident] -> Bool
reduceOnPointerParameters'' _ [] [] = True
reduceOnPointerParameters'' pg (x:xs) (y:ys)
    | getAlphaTypeName (aliases pg) x == (return y) = reduceOnPointerParameters'' pg xs ys
    | otherwise = False

reduceParameterTypes::ProgramEnvironment -> [Set Ident] -> [SourcePosition Expression] -> CompilerStatus [(Either (Set Ident) (Ident, String))]
reduceParameterTypes programEnvironment possibleParameterTypes parameterExpressions = do
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


zipVariablesToParameterTypesAndExpressions::[(Ident, Variable)]->[String]->[SourcePosition Expression]->CompilerStatus [((Ident, Variable), String, SourcePosition Expression)]
zipVariablesToParameterTypesAndExpressions [] [] [] = return []
zipVariablesToParameterTypesAndExpressions ((_, (VVariadic _ _)):[]) [] [] = return []
zipVariablesToParameterTypesAndExpressions (x@(_, paramVar@(VVariadic _ _)):[]) (y:ys) (z:zs) = do
    rest <- zipVariablesToParameterTypesAndExpressions (x:[]) ys zs
    return $ (x, y, z):rest
zipVariablesToParameterTypesAndExpressions xs@((x@(_, (VVariadic _ _)):_)) ys (z:zs) = 
    PositionedError (getSourcePosition z) "Variadic parameters must only be placed at the end of a parameter list" (show (xs, ys, z:zs))
zipVariablesToParameterTypesAndExpressions (x:xs) (y:ys) (z:zs) = do
    rest <- zipVariablesToParameterTypesAndExpressions xs ys zs
    return $ (x, y, z):rest
    
generateExpressionForPointerParameter::ProgramEnvironment -> String -> SourcePosition Expression -> CompilerStatus String
generateExpressionForPointerParameter programEnvironment generatedExpression spExp = case getContents spExp of
    EIdent varName -> do
        variable <- lookupVariableType programEnvironment varName
        case variable of
            (VPointer _ _) -> return generatedExpression
            (VConst _ _) -> return $ "&" ++ generatedExpression
    expr -> PositionedError (getSourcePosition spExp) ((show expr) ++ " cannot be referenced in a pointer context") (show expr)

generateDFunctionCall::ProgramEnvironment -> Ident -> FunctionOverload -> [Either (Set Ident) (Ident, String)] -> [SourcePosition Expression] -> CompilerStatus String
generateDFunctionCall programEnvironment funcName functionOverload paramTypeEithers parameterExpressions = do
    generatedParameters <- sequence $ Prelude.map (either (\x -> Error "Parameter expression could not be generated" (show (x, (funcName, functionOverload)))) (\(_,x) -> return x)) paramTypeEithers
    zippedParameters <- (zipVariablesToParameterTypesAndExpressions (OrderMap.assocs (parameters functionOverload)) generatedParameters parameterExpressions)
    addressWrappedParameters <- sequence $ Prelude.map (\((_,x),y, z) -> addressWrapper x y z) zippedParameters
    generateDFunctionCall' programEnvironment funcName functionOverload addressWrappedParameters
    where
        addressWrapper (VPointer _ _) x y = generateExpressionForPointerParameter programEnvironment x y
        addressWrapper (VConst _ _) x y = generateExpressionForConstParameter programEnvironment x y
        addressWrapper (VVariadic _ _) x y = generateExpressionForConstParameter programEnvironment x y

generateDFunctionCall'::ProgramEnvironment->Ident->FunctionOverload->[String]->CompilerStatus String
generateDFunctionCall' programEnvironment funcName (FO { returnType=returnType, parameters=parameters }) parameterExpressions = do
    parameterTypes <- sequence $ [ getAlphaTypeName (aliases programEnvironment) x | (_, x) <- OrderMap.assocs parameters ]
    let fullDName = (case functionIsNative programEnvironment funcName of
            (Just postfix) -> dropPostfix postfix funcName
            Nothing -> let dFuncSuffix = returnType ++ (concat parameterTypes) in funcName ++ "_" ++ dFuncSuffix) in
        let joinedParameterExpressions = concat $ intersperse "," parameterExpressions in
            return $ fullDName ++ "(" ++ joinedParameterExpressions ++ ")"
    where
    

dropPostfix postfix = (reverse.(Prelude.drop $ length postfix).reverse)

functionIsNative:: ProgramEnvironment -> Ident -> Maybe String
functionIsNative (PE { functions = funcEnvironment }) funcName
    | ((dropPostfix "_function" funcName) `Map.member` funcEnvironment) = return "_function"
    | ((dropPostfix "_operator" funcName) `Map.member` funcEnvironment) = return "_operator"
    | otherwise = Nothing
