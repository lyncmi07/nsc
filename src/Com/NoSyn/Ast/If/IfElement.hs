module Com.NoSyn.Ast.If.IfElement where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.If.Constant
import Com.NoSyn.Ast.If.Expression
import Com.NoSyn.Ast.If.AliasDefinition
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.Parameter
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.PreProgram
import Com.NoSyn.Ast.If.Statement
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.ImportStatement
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Error.SourcePosition

data IfElement =
    IfConstant (SourcePosition Constant)
    | IfExpression (SourcePosition Expression)
    | IfAliasDefinition (SourcePosition AliasDefinition)
    | IfFunctionDefinition (SourcePosition FunctionDefinition)
    | IfParameter (SourcePosition Parameter)
    | IfProgram (SourcePosition Program)
    | IfPreProgram (SourcePosition PreProgram)
    | IfProgramStmt (SourcePosition ProgramStmt)
    | IfStatement (SourcePosition Statement)
    | IfVariableDeclaration (SourcePosition VariableDeclaration)
    | IfParameters (SourcePosition Parameters)
    | IfBlockStatement (SourcePosition BlockStatement)
    | IfImportStatement (SourcePosition ImportStatement)
    | IfImportStatements (SourcePosition ImportStatements)
    deriving Show

instance TargetCodeGeneratable IfElement where
    generateD a spIfElement = case getContents spIfElement of
        IfConstant b -> generateD a b
        IfExpression b -> generateD a b
        IfAliasDefinition b -> generateD a b
        IfFunctionDefinition b -> generateD a b
        IfParameter b -> generateD a b
        IfProgram b -> generateD a b
        IfPreProgram b -> generateD a b
        IfProgramStmt b -> generateD a b
        IfStatement b -> generateD a b
        IfVariableDeclaration b -> generateD a b
        IfParameters b -> generateD a b
        IfBlockStatement b -> generateD a b
        IfImportStatement b -> generateD a b
        IfImportStatements b -> generateD a b
