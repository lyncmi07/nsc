module Com.NoSyn.Evaluation.Program.Internal.FunctionEvaluation (programFunctionDefinitionEvaluate) where

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
    (functionName, newFunction) <- createFunctionEntry aliasEnvironment funcDef
    let currentFunctionOverloads = fromMaybe [] $ Data.Map.lookup functionName currentFunctionEnvironment in
        let newFunctionEnvironment = insert functionName (newFunction:currentFunctionOverloads) currentFunctionEnvironment in
            programFunctionDefinitionEvaluate' aliasEnvironment xs newFunctionEnvironment
programFunctionDefinitionEvaluate' aliasEnvironment (_:xs) currentFunctionEnvironment =
    programFunctionDefinitionEvaluate' aliasEnvironment xs currentFunctionEnvironment

createFunctionEntry::AliasEnvironment -> FunctionDefinition -> CompilerStatus (Ident, FunctionOverload)
createFunctionEntry aliasEnvironment (FDNative functionName returnType parameters)= do
    _ <- lookupDType returnType aliasEnvironment
    _ <- sequence $ Prelude.map (\(_, x) -> lookupDType (getTypeNoCheck x) aliasEnvironment) (parametersToTuples parameters)
    return (functionName, FO { returnType = returnType, parameters = parameterOMap, parentModule = Nothing })
    where
        parameterOMap = (Data.Map.Ordered.fromList (parametersToTuples parameters))
createFunctionEntry aliasEnvironment (FDNoSyn functionName returnType parameters _)= do
    _ <- lookupDType returnType aliasEnvironment
    _ <- sequence $ Prelude.map (\(_, x) -> lookupDType (getTypeNoCheck x) aliasEnvironment) (parametersToTuples parameters)
    return (functionName, FO { returnType = returnType, parameters = parameterOMap, parentModule = Nothing})
    where
        parameterOMap = (Data.Map.Ordered.fromList (parametersToTuples parameters))

