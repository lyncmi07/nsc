{-# LANGUAGE MultiParamTypeClasses #-}
module Com.NoSyn.Ast.If.VariableDeclaration where

import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.DIdentifiable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Environment.ProgramEnvironment
import Data.Map

data VariableDeclaration = VDec Ident Ident
    deriving Show

instance TargetCodeGeneratable VariableDeclaration where
    generateD programEnvironment varDec@(VDec varType varName) = do
        dType <- (getRealType programEnvironment varDec)
        return $ dType ++ " " ++ varName

instance DIdentifiable VariableDeclaration where
    getDIdentifier _ (VDec _ varName) = return varName
instance EnvironmentUpdater VariableDeclaration where
    updateEnvironment programEnvironment@(PG {variables = variableEnvironment}) varDec@(VDec _ varName) = do
        varType <- getNoSynType programEnvironment varDec
        let updatedVariableEnvironment = insert varName (VConst varType varName) variableEnvironment in
            return (programEnvironment { variables = updatedVariableEnvironment })

instance Typeable VariableDeclaration where
    getTypeNoCheck (VDec varType _) = varType
