module Main where

import System.Environment

import Lib
import Com.NoSyn.Parser.NoSynParser
import Com.NoSyn.Parser.Lexer
import Com.NoSyn.Parser.ConcreteSyntaxConverter
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.IOConvertable
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Evaluation.Program.Program

main = do
    (x:_) <- getArgs
    programText <- readFile x
    tokens <- return $ lexer programText
    cst <- return $ parse tokens
    ifm1Ast <- toIO $ convertProgram cst
    ifAst <- toIO $ generateIfElement defaultProgramEnvironment ifm1Ast
    initialProgramEnvironment <- toIO $ programEnvironmentEvaluateIfElement ifAst
    targetCode <- toIO $ generateD initialProgramEnvironment ifAst
    putStrLn targetCode
