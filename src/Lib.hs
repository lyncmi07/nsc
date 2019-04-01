module Lib where

import Com.NoSyn.Ast.If.Program as IfProgram
import Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.If.FunctionDefinition as IfFunctionDefinition
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.Statement as IfStatement
import Com.NoSyn.Ast.If.VariableDeclaration as IfVariableDeclaration
import Com.NoSyn.Ast.If.Expression as IfExpression
import Com.NoSyn.Ast.If.Constant as IfConstant

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Evaluation.Program.Program
import Com.NoSyn.Environment.ProgramEnvironment

import Com.NoSyn.Ast.Ifm1.Program as Ifm1Program
import Com.NoSyn.Ast.Ifm1.Parameter as Ifm1Parameter
import Com.NoSyn.Ast.Ifm1.FunctionDefinition as Ifm1FunctionDefinition
import Com.NoSyn.Ast.Ifm1.Statement as Ifm1Statement
import Com.NoSyn.Ast.Ifm1.VariableDeclaration as Ifm1VariableDeclaration
import Com.NoSyn.Ast.Ifm1.Expression as Ifm1Expression
import Com.NoSyn.Ast.Ifm1.Constant as Ifm1Constant

import Com.NoSyn.Ast.If.IfElement as IfElement
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Data.Operators

ifExampleProgram = StandardBlock [
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Int" (StandardBlock [
            IfParameter.PPointer "Int" "a",
            IfParameter.PConst "Double" "b" 
           ]) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Double" (StandardBlock [
            IfParameter.PConst "Int" "a",
            IfParameter.PConst "Double" "b" 
           ]) (SequentialBlock [
                IfStatement.SVarDec (IfVariableDeclaration.VDec "Int" "n"),
                IfStatement.SVarDec (IfVariableDeclaration.VDec "Double" "m"),
                --SExpression (EFuncCall "foo" [EFuncCall "foo" [EConst (CInt 5), EConst (CDouble 25.4)], EIdent "b"])
                IfStatement.SExpression (IfExpression.EFuncCall "foo" [IfExpression.EIdent "a", IfExpression.EIdent "b"])
            ])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [
                IfParameter.PConst "Int" "a",
                IfParameter.PConst "Double" "b"
            ]) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [
                IfParameter.PConst "Double" "c",
                IfParameter.PConst "Double" "d"
            ]) (SequentialBlock []))
    ]
    
functionInferenceProgram = StandardBlock [
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [IfParameter.PConst "Int" "a", IfParameter.PConst "Double" "b" ]) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [IfParameter.PConst "Int" "a", IfParameter.PConst "Char" "b" ]) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [IfParameter.PConst "Double" "a", IfParameter.PConst "Int" "b" ]) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "bar" "Int" (StandardBlock []) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "bar" "Char" (StandardBlock []) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "cello" "Int" (StandardBlock []) (SequentialBlock [])),
        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "cello" "Double" (StandardBlock []) (SequentialBlock [])),


        IfProgram.PSFuncDef (IfFunctionDefinition.FDNoSyn "cello" "Nothing" (StandardBlock []) (SequentialBlock [
                IfStatement.SExpression (IfExpression.EFuncCall "foo" [IfExpression.EFuncCall "bar" [], IfExpression.EFuncCall "cello" []])
            ]))
    ]

ifm1ExampleProgram = StandardBlock [
        Ifm1Program.PSFuncDef (Ifm1FunctionDefinition.FDNoSyn "foo" "Int" (StandardBlock [
            Ifm1Parameter.IfParameter $ IfParameter.PPointer "Int" "a",
            Ifm1Parameter.IfParameter $ IfParameter.PConst "Double" "b" 
           ]) (SequentialBlock [])),
        Ifm1Program.PSFuncDef (Ifm1FunctionDefinition.FDNoSyn "foo" "Double" (StandardBlock [
            Ifm1Parameter.IfParameter $ IfParameter.PConst "Int" "a",
            Ifm1Parameter.IfParameter $ IfParameter.PConst "Double" "b" 
           ]) (SequentialBlock [
                Ifm1Statement.SVarDec (Ifm1VariableDeclaration.IfVariableDeclaration $ IfVariableDeclaration.VDec "Int" "n"),
                Ifm1Statement.SVarDec (Ifm1VariableDeclaration.IfVariableDeclaration $ IfVariableDeclaration.VDec "Double" "m"),
                --SExpression (EFuncCall "foo" [EFuncCall "foo" [EConst (CInt 5), EConst (CDouble 25.4)], EIdent "b"])
                Ifm1Statement.SExpression (Ifm1Expression.EFuncCall "foo" [Ifm1Expression.EIdent "a", Ifm1Expression.EIdent "b"]),
                Ifm1Statement.SExpression (Ifm1Expression.EOp Prefix "++" [Ifm1Expression.EIdent "a"])
            ])),
        Ifm1Program.PSFuncDef (Ifm1FunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [
                Ifm1Parameter.IfParameter $ IfParameter.PConst "Int" "a",
                Ifm1Parameter.IfParameter $ IfParameter.PConst "Double" "b"
            ]) (SequentialBlock [])),
        Ifm1Program.PSFuncDef (Ifm1FunctionDefinition.FDNoSyn "foo" "Nothing" (StandardBlock [
                Ifm1Parameter.IfParameter $ IfParameter.PConst "Double" "c",
                Ifm1Parameter.IfParameter $ IfParameter.PConst "Double" "d"
            ]) (SequentialBlock [])),
        Ifm1Program.PSFuncDef (Ifm1FunctionDefinition.FDOperatorOverload Postfix "++" "Nothing" (StandardBlock [
                Ifm1Parameter.IfParameter $ IfParameter.PPointer "Int" "a"
            ]) (SequentialBlock []))
    ]
generateIfExampleProgram :: CompilerStatus String
generateIfExampleProgram = do
    initialProgramEnvironment <- programEnvironmentEvaluate defaultProgramEnvironment ifExampleProgram
    generateD initialProgramEnvironment ifExampleProgram

generateIfm1ExampleProgram :: CompilerStatus String
generateIfm1ExampleProgram = do
    ~(IfElement.IfProgram ifProgram) <- generateIfElement defaultProgramEnvironment ifm1ExampleProgram
    initialProgramEnvironment <- programEnvironmentEvaluate defaultProgramEnvironment ifProgram
    generateD initialProgramEnvironment ifProgram

someFunc :: IO ()
someFunc = putStrLn "someFunc"
