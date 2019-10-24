{-# LANGUAGE MultiParamTypeClasses #-}
module Com.NoSyn.Ast.If.VariableDeclaration where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.DIdentifiable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Environment.ProgramEnvironment
import Data.Map

data VariableDeclaration = VDec Ident Ident
    deriving Show

instance TargetCodeGeneratable VariableDeclaration where
    generateD programEnvironment spVarDec = case getContents spVarDec of
        VDec varType varName -> do
            dType <- (getRealType programEnvironment $ getContents spVarDec)
            return $ dType ++ " " ++ varName

instance DIdentifiable VariableDeclaration where
    getDIdentifier _ (VDec _ varName) = return varName
instance EnvironmentUpdater VariableDeclaration where
    updateEnvironment programEnvironment@(PE {variables = variableEnvironment}) spVarDec = case getContents spVarDec of
        VDec _ varName -> do
            varType <- getNoSynType programEnvironment $ getContents spVarDec
            let updatedVariableEnvironment = insert varName (VConst varType varName) variableEnvironment in
                return (programEnvironment { variables = updatedVariableEnvironment })

instance Typeable VariableDeclaration where
    getTypeNoCheck (VDec varType _) = varType
    getAlphaTypeName aliasEnvironment (VDec varType _) = do
        atomicVarType <- lookupAtomicNoSynType varType aliasEnvironment
        return atomicVarType
