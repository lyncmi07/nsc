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
    generateD a (IfConstant b) = generateD a $ getContents b
    generateD a (IfExpression b) = generateD a $ getContents b
    generateD a (IfAliasDefinition b) = generateD a $ getContents b
    generateD a (IfFunctionDefinition b) = generateD a $ getContents b
    generateD a (IfParameter b) = generateD a $ getContents b
    generateD a (IfProgram b) = generateD a $ getContents b
    generateD a (IfPreProgram b) = generateD a $ getContents b
    generateD a (IfProgramStmt b) = generateD a $ getContents b
    generateD a (IfStatement b) = generateD a $ getContents b
    generateD a (IfVariableDeclaration b) = generateD a $ getContents b
    generateD a (IfParameters b) = generateD a $ getContents b
    generateD a (IfBlockStatement b) = generateD a $ getContents b
    generateD a (IfImportStatement b) = generateD a $ getContents b
    generateD a (IfImportStatements b) = generateD a $ getContents b
