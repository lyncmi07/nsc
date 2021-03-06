module Com.NoSyn.Ast.If.Parameter where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Blockable
import Com.NoSyn.Ast.Traits.Listable as Listable
import Data.List
import Data.Map
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionTraits

type Parameters = Block Parameter

data Parameter = 
    PConst Ident Ident
    | PPointer Ident Ident
    | PVariadic Ident Ident
    deriving Show

instance EnvironmentUpdater Parameter where
    updateEnvironment programEnvironment@(PE {variables = variableEnvironment}) spParam = let param = getContents spParam in
        case param of
            PConst paramType paramName -> do
                verifiedType <- getNoSynType programEnvironment param
                return (programEnvironment { variables = Data.Map.insert paramName (VConst verifiedType paramName) variableEnvironment})
            PPointer paramType paramName -> do
                verifiedType <- getNoSynType programEnvironment param
                return (programEnvironment { variables = Data.Map.insert paramName (VPointer verifiedType paramName) variableEnvironment})
            PVariadic paramType paramName -> do
                verifiedType <- getNoSynType programEnvironment param
                return (programEnvironment { variables = Data.Map.insert paramName (VVariadic verifiedType paramName) variableEnvironment})

instance TargetCodeGeneratable Parameter where
    generateD programEnvironment spParameter = let parameter = getContents spParameter in
        case parameter of
            PConst paramType paramName -> do
                parameterDType <- getRealType programEnvironment parameter
                return $ parameterDType ++ " " ++ paramName
            PPointer paramType paramName -> do
                parameterDType <- getRealType programEnvironment parameter
                return $ parameterDType ++ "* " ++ paramName
            PVariadic paramType paramName -> do
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

instance Blockable Parameter where
    blockSeparator _ = ", "

parameterToTuple::AliasEnvironment -> SourcePosition Parameter -> CompilerStatus (Ident, Variable)
parameterToTuple aliasEnvironment spParameter = case getContents spParameter of
    PConst paramType paramName -> do
        realParamType <- lookupAtomicNoSynType paramType aliasEnvironment
        return (paramName , (VConst realParamType paramName))
    PPointer paramType paramName -> do
        realParamType <- lookupAtomicNoSynType paramType aliasEnvironment
        return (paramName , (VPointer realParamType paramName))
    PVariadic paramType paramName -> do
        realParamType <- lookupAtomicNoSynType paramType aliasEnvironment
        return (paramName, (VVariadic realParamType paramName))

parametersToTuples::AliasEnvironment -> SourcePosition Parameters -> CompilerStatus [(Ident, Variable)]
parametersToTuples aliasEnvironment parameters =
    sequence $ Prelude.map (parameterToTuple aliasEnvironment) (toSourcePositionedList $ getContents parameters)
