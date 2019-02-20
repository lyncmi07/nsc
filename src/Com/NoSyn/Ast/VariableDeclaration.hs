module Com.NoSyn.Ast.VariableDeclaration where

import Com.NoSyn.Ast.Typeable
import Com.NoSyn.Ast.TypeCheckFunctions
import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Ast.DIdentifiable
import Com.NoSyn.Ast.EnvironmentUpdater
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Environment.ProgramEnvironment
import Data.Map

data VariableDeclaration = VDec Ident Ident

instance AstElement VariableDeclaration where
    generateD programEnvironment varDec@(VDec varType varName) = do
        dType <- (getRealType programEnvironment varDec)
        return $ dType ++ " " ++ varName
instance DIdentifiable VariableDeclaration where
    getDIdentifier _ (VDec _ varName) = return varName
instance EnvironmentUpdater VariableDeclaration where
    updateEnvironment programEnvironment@(ae, fe, variableEnvironment) varDec@(VDec _ varName) = do
        varType <- getNoSynType programEnvironment varDec
        let updatedVariableEnvironment = insert varName (VConst varType varName) variableEnvironment in
            return (ae, fe, updatedVariableEnvironment)
        
instance Typeable VariableDeclaration where
    getTypeNoCheck (VDec varType _) = varType
