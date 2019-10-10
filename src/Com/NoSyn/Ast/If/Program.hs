module Com.NoSyn.Ast.If.Program where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.AliasDefinition
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.SourcePosition

type Program = Block (SourcePosition ProgramStmt)
data ProgramStmt =
    PSAliasDef (SourcePosition AliasDefinition)
    | PSFuncDef (SourcePosition FunctionDefinition)
    | PSVarDec (SourcePosition VariableDeclaration)
    deriving Show

instance TargetCodeGeneratable ProgramStmt where
    generateD _ (PSAliasDef _) = return "";
    generateD programEnvironment (PSFuncDef functionDefinition) = generateD programEnvironment (getContents functionDefinition)
    generateD programEnvironment (PSVarDec variableDeclaration) = generateD programEnvironment (getContents variableDeclaration)

instance EnvironmentUpdater ProgramStmt where
    updateEnvironment programEnvironment (PSAliasDef aliasDefinition) =
        updateEnvironment programEnvironment (getContents aliasDefinition)
instance Blockable ProgramStmt where
    blockSeparator _ = ";\n"
