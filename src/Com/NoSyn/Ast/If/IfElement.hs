module Com.NoSyn.Ast.If.IfElement where

import Com.NoSyn.Ast.If.Constant
import Com.NoSyn.Ast.If.Expression
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.Parameter
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.Statement
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable

data IfElement = 
    IfConstant Constant
    | IfExpression Expression
    | IfFunctionDefinition FunctionDefinition
    | IfParameter Parameter
    | IfProgram Program
    | IfStatement Statement
    | IfVariableDeclaration VariableDeclaration

instance TargetCodeGeneratable IfElement where
    generateD a (IfConstant b) = generateD a b
    generateD a (IfExpression b) = generateD a b
    generateD a (IfFunctionDefinition b) = generateD a b
    generateD a (IfParameter b) = generateD a b
    generateD a (IfProgram b) = generateD a b
    generateD a (IfStatement b) = generateD a b
    generateD a (IfVariableDeclaration b) = generateD a b
