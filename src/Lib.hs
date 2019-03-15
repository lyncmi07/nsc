module Lib where

import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.Parameter
import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.Statement
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Expression
import Com.NoSyn.Ast.If.Constant
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Evaluation.Program.Program
import Com.NoSyn.Environment.ProgramEnvironment

exampleProgram = StandardBlock [
        PSFuncDef (FDNoSyn "foo" "Int" (StandardBlock [
            PPointer "Int" "a",
            PConst "Double" "b" 
           ]) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "foo" "Double" (StandardBlock [
            PConst "Int" "a",
            PConst "Double" "b" 
           ]) (SequentialBlock [
                SVarDec (VDec "Int" "n"),
                SVarDec (VDec "Double" "m"),
                --SExpression (EFuncCall "foo" [EFuncCall "foo" [EConst (CInt 5), EConst (CDouble 25.4)], EIdent "b"])
                SExpression (EFuncCall "foo" [EIdent "b", EIdent "a"])
            ])),
        PSFuncDef (FDNoSyn "foo" "Nothing" (StandardBlock [
                PConst "Int" "a",
                PConst "Double" "b"
            ]) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "foo" "Nothing" (StandardBlock [
                PConst "Double" "c",
                PConst "Double" "d"
            ]) (SequentialBlock []))
    ]
    
functionInferenceProgram = StandardBlock [
        PSFuncDef (FDNoSyn "foo" "Nothing" (StandardBlock [PConst "Int" "a", PConst "Double" "b" ]) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "foo" "Nothing" (StandardBlock [PConst "Int" "a", PConst "Char" "b" ]) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "foo" "Nothing" (StandardBlock [PConst "Double" "a", PConst "Int" "b" ]) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "bar" "Int" (StandardBlock []) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "bar" "Char" (StandardBlock []) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "cello" "Int" (StandardBlock []) (SequentialBlock [])),
        PSFuncDef (FDNoSyn "cello" "Double" (StandardBlock []) (SequentialBlock [])),


        PSFuncDef (FDNoSyn "cello" "Nothing" (StandardBlock []) (SequentialBlock [
                SExpression (EFuncCall "foo" [EFuncCall "bar" [], EFuncCall "cello" []])
            ]))
    ]

generateExampleProgram :: CompilerStatus String
generateExampleProgram = do
    initialProgramEnvironment <- programEnvironmentEvaluate defaultProgramEnvironment exampleProgram
    generateD initialProgramEnvironment exampleProgram
    

someFunc :: IO ()
someFunc = putStrLn "someFunc"
