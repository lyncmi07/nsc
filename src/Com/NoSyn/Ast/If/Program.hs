module Com.NoSyn.Ast.If.Program where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Blockable
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.AliasDefinition
import Com.NoSyn.Data.Types
import Data.Map.Ordered
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.SourcePosition

type Program = Block ProgramStmt
data ProgramStmt =
    PSAliasDef (SourcePosition AliasDefinition)
    | PSFuncDef (SourcePosition FunctionDefinition)
    | PSVarDec (SourcePosition VariableDeclaration)
    deriving Show

instance TargetCodeGeneratable ProgramStmt where
    generateD programEnvironment spProgramStmt = case getContents spProgramStmt of
        PSAliasDef _ -> return ""
        PSFuncDef functionDefinition -> generateD programEnvironment functionDefinition
        PSVarDec variableDeclaration -> generateD programEnvironment variableDeclaration

instance EnvironmentUpdater ProgramStmt where
    updateEnvironment programEnvironment spProgramStmt = case getContents spProgramStmt of
        PSAliasDef aliasDefinition -> updateEnvironment programEnvironment aliasDefinition
instance Blockable ProgramStmt where
    blockSeparator _ = ";\n"
