module Com.NoSyn.Evaluation.Program.Internal.FunctionEvaluation (programFunctionDefinitionEvaluate) where

import Prelude hiding (getContents)
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.Parameter
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Data.Map
import Data.Map.Ordered
import Data.Maybe
import Com.NoSyn.Ast.Traits.Typeable

type Param = (Ident, Ident)

programFunctionDefinitionEvaluate::AliasEnvironment -> Program -> CompilerStatus FunctionEnvironment
programFunctionDefinitionEvaluate aliasEnvironment program =
    programFunctionDefinitionEvaluate' aliasEnvironment (Listable.toList program) Data.Map.empty

programFunctionDefinitionEvaluate'::AliasEnvironment->[ProgramStmt]->FunctionEnvironment->CompilerStatus FunctionEnvironment
programFunctionDefinitionEvaluate' _ [] functionLookup = return functionLookup
programFunctionDefinitionEvaluate' aliasEnvironment ((PSFuncDef funcDef):xs) currentFunctionEnvironment = do
    (functionName, newFunction) <- createFunctionEntry aliasEnvironment (getContents funcDef)
    let currentFunctionOverloads = fromMaybe [] $ Data.Map.lookup functionName currentFunctionEnvironment in
        if any (==newFunction) currentFunctionOverloads then
            Error ("There is already an equivalent function to " ++ (show newFunction)) (show currentFunctionOverloads)
        else let newFunctionEnvironment = insert functionName (newFunction:currentFunctionOverloads) currentFunctionEnvironment in
            programFunctionDefinitionEvaluate' aliasEnvironment xs newFunctionEnvironment
programFunctionDefinitionEvaluate' aliasEnvironment (_:xs) currentFunctionEnvironment =
    programFunctionDefinitionEvaluate' aliasEnvironment xs currentFunctionEnvironment

createFunctionEntry::AliasEnvironment -> FunctionDefinition -> CompilerStatus (Ident, FunctionOverload)
createFunctionEntry aliasEnvironment (FDNative functionName returnType parameters)= do
    _ <- lookupDType returnType aliasEnvironment
    unaliasedParameterList <- parametersToTuples aliasEnvironment parameters
    unaliasedReturnType <- lookupAtomicNoSynType returnType aliasEnvironment
    _ <- sequence $ Prelude.map (\(_, x) -> lookupDType (getTypeNoCheck x) aliasEnvironment) unaliasedParameterList
    return (functionName, FO { returnType = unaliasedReturnType, parameters = (Data.Map.Ordered.fromList unaliasedParameterList), parentModule = Nothing })
createFunctionEntry aliasEnvironment (FDNoSyn functionName returnType parameters _)= do
    _ <- lookupDType returnType aliasEnvironment
    unaliasedParameterList <- parametersToTuples aliasEnvironment parameters
    unaliasedReturnType <- lookupAtomicNoSynType returnType aliasEnvironment
    _ <- sequence $ Prelude.map (\(_, x) -> lookupDType (getTypeNoCheck x) aliasEnvironment) unaliasedParameterList
    return (functionName, FO { returnType = unaliasedReturnType, parameters = (Data.Map.Ordered.fromList unaliasedParameterList), parentModule = Nothing})


