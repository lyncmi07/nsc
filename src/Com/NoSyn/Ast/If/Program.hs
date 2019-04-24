module Com.NoSyn.Ast.If.Program where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Com.NoSyn.Environment.ProgramEnvironment

type Program = Block ProgramStmt
data ProgramStmt =
    PSAliasDef Ident Ident
    | PSFuncDef FunctionDefinition
    | PSVarDec VariableDeclaration
    deriving Show

instance TargetCodeGeneratable ProgramStmt where
    generateD _ (PSAliasDef _ _) = return "";
    generateD programEnvironment (PSFuncDef functionDefinition) = generateD programEnvironment functionDefinition
    generateD programEnvironment (PSVarDec variableDeclaration) = generateD programEnvironment variableDeclaration

instance EnvironmentUpdater ProgramStmt where
    updateEnvironment programEnvironment@(PE { aliases = aliasEnvironment }) (PSAliasDef aliasName aliasType) = do
        _ <- lookupDType aliasType aliasEnvironment
        let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
            return (programEnvironment { aliases = updatedAliasEnvironment })
instance Blockable ProgramStmt where
    blockSeparator _ = ";\n"
