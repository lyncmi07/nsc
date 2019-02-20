module Com.NoSyn.Ast.Program where

import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Ast.FunctionDefinition
import Com.NoSyn.Ast.VariableDeclaration
import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.Block
import Com.NoSyn.Ast.EnvironmentUpdater
import Data.Map.Ordered
import Com.NoSyn.Environment.ProgramEnvironment

type Program = Block ProgramStmt
data ProgramStmt =
    PSAliasDef Ident Ident
    | PSFuncDef FunctionDefinition
    | PSVarDec VariableDeclaration

instance AstElement ProgramStmt where
    generateD _ (PSAliasDef _ _) = return "";
    generateD programEnvironment (PSFuncDef functionDefinition) = generateD programEnvironment functionDefinition
    generateD programEnvironment (PSVarDec variableDeclaration) = generateD programEnvironment variableDeclaration
instance EnvironmentUpdater ProgramStmt where
    updateEnvironment programEnvironment@(aliasEnvironment, fe, ve) (PSAliasDef aliasName aliasType) = do
        _ <- lookupDType aliasType aliasEnvironment
        let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
            return (updatedAliasEnvironment, fe, ve)
instance Blockable ProgramStmt where
    blockSeparator _ = ";\n"
