module Main where

import System.Environment

import Com.NoSyn.Parser.NoSynParser
import Com.NoSyn.Parser.Lexer
import Com.NoSyn.Parser.ConcreteSyntaxConverter
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition hiding (getContents)
import Com.NoSyn.Error.SourcePositionTraits
import qualified Com.NoSyn.Error.SourcePosition as SourcePosition (getContents)
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
import Com.NoSyn.Ast.If.Block
import Data.Foldable

main = do
    args <- getArgs
    (headerText, programText) <- Prelude.getContents >>= return.splitInputs
    (_, cst) <- convertToIO $ failOnNonFatalErrors programText (parse programText 0 1 [])
    (_, positionedIfm1Ast) <- convertToIOWithSourcePositions programText $ 
        let positionedIfAstResult = runCompilerStatusT $ convertProgram cst in do
            ifm1Ast <- SourcePosition.getContents positionedIfAstResult
            return $ changeContents positionedIfAstResult ifm1Ast
    (_, positionedIfAst) <- convertToIOWithSourcePositions programText $ generateIfElement defaultProgramEnvironment positionedIfm1Ast
    if args == [] then compileProgram programText headerText positionedIfAst positionedIfm1Ast
    else let (x:_) = args in
        if x == "--headers" then createHeaders positionedIfAst
        else putStrLn $ "Invalid argument: " ++ x

compileProgram sourceCode headerText spInitialIfAst spIfm1Ast = case SourcePosition.getContents spInitialIfAst of
    IfPreProgram spcIfPreProgram -> case SourcePosition.getContents spcIfPreProgram of
        IfPreProgram.PreProgram allImports _ -> do
            (_, initialProgramEnvironment) <- convertToIOWithSourcePositions sourceCode $ programEnvironmentEvaluateIfElement (SourcePosition.getContents spInitialIfAst)
            -- Now that the program environment has been populated the ifAst is generated again to correct any missing function calls from expressions
            selectedImports <- return $ filteredHeaders headerText (Prelude.map SourcePosition.getContents $ toSourcePositionedList $ SourcePosition.getContents allImports)
            (_, programEnvironmentWithImports) <- convertToIOWithSourcePositions sourceCode $ addImportedFunctionsToEnvironment selectedImports initialProgramEnvironment
            (_, ifAst) <- convertToIOWithSourcePositions sourceCode $ generateIfElement programEnvironmentWithImports spIfm1Ast
            (dependencies, targetCode) <- convertToIOWithSourcePositions sourceCode $ generateD programEnvironmentWithImports ifAst
            putStrLn (concat $ intersperse "\n" dependencies)
            putStrLn "%%SOURCE%%"
            putStrLn targetCode

createHeaders ifAst = do
    (_, functionEnvironment) <- convertToIO $ functionEnvironmentEvaluateIfElement $ SourcePosition.getContents ifAst
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
        importNameProvider (NativeImport a) = concat $ intersperse "." (Prelude.map SourcePosition.getContents a)
        importNameProvider (NSImport a) = concat $ intersperse "." (Prelude.map SourcePosition.getContents a)
