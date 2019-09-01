module Com.NoSyn.Ast.If.Parameter where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Listable as Listable
import Data.List
import Data.Map
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus

type Parameters = Block Parameter


data Parameter = 
    PConst Ident Ident
    | PPointer Ident Ident
    | PVariadic Ident Ident
    deriving Show

instance EnvironmentUpdater Parameter where
    updateEnvironment programEnvironment@(PE {variables = variableEnvironment}) param@(PConst paramType paramName) = do
        verifiedType <- getNoSynType programEnvironment param
        return (programEnvironment { variables = Data.Map.insert paramName (VConst verifiedType paramName) variableEnvironment})
    updateEnvironment programEnvironment@(PE { variables = variableEnvironment}) param@(PPointer paramType paramName) = do
        verifiedType <- getNoSynType programEnvironment param
        return (programEnvironment { variables = Data.Map.insert paramName (VPointer verifiedType paramName) variableEnvironment})
    updateEnvironment programEnvironment@(PE { variables = variableEnvironment}) param@(PVariadic paramType paramName) = do
        verifiedType <- getNoSynType programEnvironment param
        return (programEnvironment { variables = Data.Map.insert paramName (VVariadic verifiedType paramName) variableEnvironment})

instance TargetCodeGeneratable Parameter where
    generateD programEnvironment parameter@(PConst paramType paramName) = do
        parameterDType <- getRealType programEnvironment parameter
        return $ parameterDType ++ " " ++ paramName
    generateD programEnvironment parameter@(PPointer paramType paramName) = do
        parameterDType <- getRealType programEnvironment parameter
        return $ parameterDType ++ "* " ++ paramName
    generateD programEnvironment parameter@(PVariadic paramType paramName) = do
        parameterDType <- getRealType programEnvironment parameter
        return $ parameterDType ++ "[] " ++ paramName ++ " ..."

instance Typeable Parameter where
    getTypeNoCheck (PConst paramType _) = paramType
    getTypeNoCheck (PPointer paramType _) = paramType ++ "*"
    getTypeNoCheck (PVariadic paramType _) = paramType ++ "..."
    getAlphaTypeName aliasEnvironment (PConst paramType _) = lookupAtomicNoSynType paramType aliasEnvironment 
    getAlphaTypeName aliasEnvironment (PPointer paramType _) = do
        atomicTypeName <- lookupAtomicNoSynType paramType aliasEnvironment 
        return $ atomicTypeName ++ "PTR"
    getAlphaTypeName aliasEnvironment (PVariadic paramType _) = do
        atomicTypeName <- lookupAtomicNoSynType paramType aliasEnvironment
        return $ atomicTypeName ++ "VARAD"

getAtomicTypeName pg paramType = lookupAtomicNoSynType  paramType

instance Blockable Parameter where
    blockSeparator _ = ", "

parameterToTuple::Parameter -> (Ident, Variable)
parameterToTuple (PConst paramType paramName) = 
    (paramName , (VConst paramType paramName))
parameterToTuple (PPointer paramType paramName) = 
    (paramName , (VPointer paramType paramName))
parameterToTuple (PVariadic paramType paramName) =
    (paramName, (VVariadic paramType paramName))

parametersToTuples::Parameters -> [(Ident, Variable)]
parametersToTuples parameters =
    Prelude.map parameterToTuple (Listable.toList parameters)
