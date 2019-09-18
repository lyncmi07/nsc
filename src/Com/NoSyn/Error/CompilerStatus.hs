module Com.NoSyn.Error.CompilerStatus where

import Com.NoSyn.Error.MaybeConvertable
import Com.NoSyn.Error.NonFatalError
import Com.NoSyn.Error.CompilerContext as CompilerContext
import Data.Set.SetTheory as SetTheory
import Data.Set (empty, singleton, toList)
import Data.List.Split
import Data.List
import System.IO


type LineNumber = Int
type Column = Int
type Cs a = String -> Column -> LineNumber -> [(LineNumber, Column, LineNumber, Column)] -> CompilerStatus a

getLineNumber :: Cs LineNumber
getLineNumber = \s _ l _ -> Valid CompilerContext.empty l

data CompilerStatus a =
    Valid CompilerContext a
    | Error String String
    deriving (Show, Eq)

instance Functor CompilerStatus where
    fmap f (Valid cc a) = Valid cc (f a)
    fmap f (Error a b) = Error a b

instance Applicative CompilerStatus where
    pure = Valid (CC {moduleDependencies = Data.Set.empty, nonFatalErrors = []})
    (Error a b) <*> _ = Error a b
    (Valid cc f) <*> a = case fmap f a of
        Valid ccv b -> Valid (SetTheory.union ccv cc) b
        Error errorMessage context -> Error errorMessage context

instance Monad CompilerStatus where
    (Valid cc a) >>= f = case (f a) of
        (Valid ccv b) -> Valid (SetTheory.union ccv cc) b
        (Error errorMessage context) -> Error errorMessage context
    (Error a b) >>= f = Error a b
    return = Valid (CC {moduleDependencies = Data.Set.empty, nonFatalErrors = []})

instance MaybeConvertable CompilerStatus where
    toMaybe (Valid _ a) = (Just a)
    toMaybe (Error _ _) = Nothing

convertToIO :: CompilerStatus a -> IO ([String], a)
convertToIO (Valid compilerContext value) = return (toList (moduleDependencies compilerContext), value)
convertToIO (Error errorMessage context) = do
    hPutStrLn stderr "--COMPILATION FAILED--"
    hPutStrLn stderr "Reason:"
    hPutStrLn stderr errorMessage
    hPutStrLn stderr "Context:"
    hPutStrLn stderr context
    fail "Exiting unsuccessfully"

compilerStatusFromMaybe::String->Maybe a->CompilerStatus a
compilerStatusFromMaybe _ (Just a) = return a
compilerStatusFromMaybe errorString _ = Error errorString "No Context"

dependencyRequired :: String -> a -> CompilerStatus a
dependencyRequired moduleName value = Valid (CC {moduleDependencies = singleton moduleName, nonFatalErrors = []}) value

addNonFatalError :: NonFatalError -> a -> CompilerStatus a
addNonFatalError error value = Valid (CC { moduleDependencies = Data.Set.empty, nonFatalErrors = [error]}) value

failOnNonFatalErrors :: String -> CompilerStatus a -> CompilerStatus a
failOnNonFatalErrors sourceCode b@(Valid compilerContext n)
    | (length $ nonFatalErrors compilerContext) == 0 = b
    | otherwise = Error (prettyPrintNonFatalErrors (splitOn "\n" sourceCode) compilerContext) "Compilation failed from too many errors"
failOnNonFatalErrors _ error = error

prettyPrintNonFatalErrors :: [String] -> CompilerContext -> String
prettyPrintNonFatalErrors sourceCodeLines (CC { nonFatalErrors = errors }) =
    concat [let relevantCode = sourceCodeLines !! (startLine - 1) in
                "\nError occured from line " ++ (show startLine) ++ " col " ++ (show startColumn) ++ " to line " ++ (show endLine) ++ " col "  ++ (show endColumn) ++ ":\n"
                ++ prettyPrintTokenPosition sourceCodeLines startLine startColumn endLine endColumn ++ "\n"
                ++ "Cause: " ++ errorMessage 
                ++ "\n\n------------------------------"
            | (NFE errorMessage _ startLine startColumn endLine endColumn) <- reverse errors]

prettyPrintTokenPosition :: [String] -> LineNumber -> Column -> LineNumber -> Column -> String
prettyPrintTokenPosition sourceLines startLine startColumn endLine endColumn =
    if (startLine == endLine) then prettyPrintSingleLinePosition sourceLines startLine startColumn endColumn
    else prettyPrintToEndOfLine sourceLines startLine startColumn ++ "\n"
        ++ prettyPrintTokenPosition sourceLines (startLine + 1) 0 endLine endColumn

prettyPrintToEndOfLine sourceLines line startColumn =
    let codeLine = sourceLines !! (line - 1) in
        codeLine ++ "\n"
        ++ (take (startColumn - 1) $ repeat ' ') ++ (take ((length codeLine) - startColumn + 1) $ repeat '^')

prettyPrintSingleLinePosition sourceLines line startColumn endColumn = 
    let codeLine = sourceLines !! (line - 1) in
        codeLine ++ "\n"
        ++ (take (startColumn - 1) $ repeat ' ') ++ (take (endColumn - startColumn + 1) $ repeat '^')
