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
import Com.NoSyn.Serialization.FunctionEnvironmentSerialization
import Data.List.Split

main = do
    args <- getArgs
    (headerText, programText) <- getContents >>= return.splitInputs
    tokens <- return $ lexer programText
    cst <- return $ parse tokens
    ifm1Ast <- toIO $ convertProgram cst
    ifAst <- toIO $ generateIfElement defaultProgramEnvironment ifm1Ast
    if args == [] then compileProgram ifAst
    else let (x:_) = args in
        if x == "--headers" then createHeaders ifAst
        else putStrLn $ "Invalid argument: " ++ x

compileProgram ifAst = do
    initialProgramEnvironment <- toIO $ programEnvironmentEvaluateIfElement ifAst
    targetCode <- toIO $ generateD initialProgramEnvironment ifAst
    putStrLn targetCode

createHeaders ifAst = do
    functionEnvironment <- toIO $ functionEnvironmentEvaluateIfElement ifAst
    putStrLn $ serializeFunctionEnvironment functionEnvironment

splitInputs standardInput = 
    (concat headerArray, concat sourceArray)
    where
        splitInputs' ("%%SOURCE%%":xs) (headerInput, _) = (headerInput, xs)
        splitInputs' (x:xs) (headerInput, _) = (headerInput ++ [x], [])
        (headerArray, sourceArray) = splitInputs' (splitOn "\n" standardInput) ([], [])
