module Main where

import System.Environment

import Lib
import Com.NoSyn.Parser.NoSynParser
import Com.NoSyn.Parser.Lexer
import Com.NoSyn.Parser.ConcreteSyntaxConverter
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Evaluation.Program.Program
import Com.NoSyn.Serialization.FunctionEnvironmentSerialization
import Data.List.Split
import Data.List
import Data.Set
import Data.Map
import Com.NoSyn.Ast.Ifm1.PreProgram as Ifm1PreProgram
import Com.NoSyn.Ast.If.PreProgram as IfPreProgram
import Com.NoSyn.Ast.Ifm1.ImportStatement as Ifm1ImportStatement
import Com.NoSyn.Ast.If.ImportStatement
import Com.NoSyn.Ast.If.IfElement
import Data.Foldable

main = do
    args <- getArgs
    (headerText, programText) <- getContents >>= return.splitInputs
    -- tokens <- return $ lexer programText
    (_, cst) <- convertToIO $ failOnNonFatalErrors programText (parse programText 1 (0, 0))
    (_, ifm1Ast@(Ifm1PreProgram.PreProgram importStatments _)) <- convertToIO $ convertProgram cst
    (_, ifAst) <- convertToIO $ generateIfElement defaultProgramEnvironment ifm1Ast
    if args == [] then compileProgram headerText ifAst ifm1Ast
    else let (x:_) = args in
        if x == "--headers" then createHeaders ifAst
        else putStrLn $ "Invalid argument: " ++ x

compileProgram headerText initialIfAst@(IfPreProgram (IfPreProgram.PreProgram allImports _)) ifm1Ast = do
    (_, initialProgramEnvironment) <- convertToIO $ programEnvironmentEvaluateIfElement initialIfAst
    -- Now that the program environment has been populated the ifAst is generated again to correct any missing function calls from expressions
    selectedImports <- return $ filteredHeaders headerText (Listable.toList allImports)
    (_, programEnvironmentWithImports) <- convertToIO $ addImportedFunctionsToEnvironment selectedImports initialProgramEnvironment
    (_, ifAst) <- convertToIO $ generateIfElement programEnvironmentWithImports ifm1Ast
    (dependencies, targetCode) <- convertToIO $ generateD programEnvironmentWithImports ifAst
    putStrLn (concat $ intersperse "\n" dependencies)
    putStrLn "%%SOURCE%%"
    putStrLn targetCode

createHeaders ifAst = do
    (_, functionEnvironment) <- convertToIO $ functionEnvironmentEvaluateIfElement ifAst
    putStrLn $ serializeFunctionEnvironment functionEnvironment

splitInputs standardInput = 
    (headerArray, concat $ intersperse "\n" sourceArray)
    where
        splitInputs' ("%%SOURCE%%":xs) (headerInput, _) = (headerInput, xs)
        splitInputs' (x:xs) (headerInput, _) = splitInputs' xs (headerInput ++ [x], [])
        (headerArray, sourceArray) = splitInputs' (splitOn "\n" standardInput) ([], [])

addImportedFunctionsToEnvironment :: [String] -> ProgramEnvironment -> CompilerStatus ProgramEnvironment
addImportedFunctionsToEnvironment headers programEnvironment@(PE {functions=functionEnvironment}) = do
    deserializedFunctionList <- sequence $ Prelude.map deserializeFunction headers
    deserializedFunctionEnvironment <- foldrM (\(x,y) n -> addFunction x y n) functionEnvironment deserializedFunctionList
    return $ programEnvironment {functions=(unifyFunctionEnvironments functionEnvironment deserializedFunctionEnvironment)}

filteredHeaders headers importStatements =
    Prelude.filter importedPredicate headers
    where
        importStatementsSet = Data.Set.fromList (importedNSModulesList importStatements)
        getModule x = Prelude.take (indexOf ':' x) x
        importedPredicate x = Data.Set.member (getModule x) importStatementsSet
        
indexOf x xs = indexOf' x xs 0
indexOf' x [] _ = -1
indexOf' x (y:ys) count
    | x == y = count
    | otherwise = indexOf' x ys (count + 1)

importedNSModulesList importStatements =
    Prelude.map importNameProvider importStatements
    where
        nsImportPredicate (NativeImport _) = False
        nsImportPredicate (NSImport _) = True
        importNameProvider (NativeImport a) = concat $ intersperse "." a
        importNameProvider (NSImport a) = concat $ intersperse "." a
