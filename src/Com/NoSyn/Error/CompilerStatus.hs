module Com.NoSyn.Error.CompilerStatus where

import Com.NoSyn.Error.MaybeConvertable
import Com.NoSyn.Error.NonFatalError
import qualified Com.NoSyn.Error.SourcePosition as SourcePosition
import Com.NoSyn.Error.CompilerContext as CompilerContext
import Data.Set.SetTheory as SetTheory
import Data.Set (empty, singleton, toList)
import Data.List.Split
import Data.List
import System.IO
import Control.Monad.Trans.Class
import Com.NoSyn.Parser.TokenLength


type LineNumber = Int
type Column = Int
type Cs a = String -> Column -> LineNumber -> [(LineNumber, Column, LineNumber, Column)] -> CompilerStatus a

getLineNumber :: Cs LineNumber
getLineNumber = \s _ l _ -> Valid CompilerContext.empty l

data CompilerStatus a =
    Valid CompilerContext a
    | Error String String
    | PositionedError SourcePosition.SourcePositionInfo String String
    deriving (Show, Eq)

instance Functor CompilerStatus where
    fmap f (Valid cc a) = Valid cc (f a)
    fmap f (Error a b) = Error a b
    fmap f (PositionedError a b c) = PositionedError a b c

instance Applicative CompilerStatus where
    pure = Valid (CC {moduleDependencies = Data.Set.empty, nonFatalErrors = []})
    (Error a b) <*> _ = Error a b
    (Valid cc f) <*> a = case fmap f a of
        Valid ccv b -> Valid (SetTheory.union ccv cc) b
        Error a b-> Error a b
        PositionedError a b c -> PositionedError a b c

instance Monad CompilerStatus where
    (Valid cc a) >>= f = case (f a) of
        (Valid ccv b) -> Valid (SetTheory.union ccv cc) b
        (Error a b) -> Error a b
        (PositionedError a b c) -> PositionedError a b c
    (Error a b) >>= f = Error a b
    (PositionedError a b c) >>= f = PositionedError a b c
    return = Valid (CC {moduleDependencies = Data.Set.empty, nonFatalErrors = []})

instance MaybeConvertable CompilerStatus where
    toMaybe (Valid _ a) = (Just a)
    toMaybe (Error _ _) = Nothing
    toMaybe (PositionedError _ _ _) = Nothing

convertToIOWithSourcePositions :: String -> CompilerStatus a -> IO ([String], a)
convertToIOWithSourcePositions _ valid@(Valid compilerContext value) = convertToIO valid
convertToIOWithSourcePositions _ err@(Error errorMessage context) = convertToIO err
convertToIOWithSourcePositions sourceCode (PositionedError (startLine, startCol, endLine, endCol) errorMessage context) = 
    let lineSplitSource = splitOn "\n" sourceCode in do
    writeErrorMessage errorMessage context
    hPutStrLn stderr "Source:"
    hPutStrLn stderr $ prettyPrintTokenPosition lineSplitSource startLine startCol endLine endCol
    fail "Exiting unsuccessfully"

convertToIO :: CompilerStatus a -> IO ([String], a)
convertToIO (Valid compilerContext value) = return (toList (moduleDependencies compilerContext), value)
convertToIO (Error errorMessage context) = do
    writeErrorMessage errorMessage context
    fail "Exiting unsuccessfully"
convertToIO (PositionedError (startLine, startCol, endLine, endCol) errorMessage context) = do
    writeErrorMessage errorMessage context
    hPutStrLn stderr "Source:"
    hPutStrLn stderr "Source not available for this error"
    fail "Exiting unsuccessfully"

writeErrorMessage errorMessage context = do
    hPutStrLn stderr "--COMPILATION FAILED--"
    hPutStrLn stderr "Reason:"
    hPutStrLn stderr errorMessage
    hPutStrLn stderr "Context:"
    hPutStrLn stderr context

compilerStatusFromMaybe::String->Maybe a->CompilerStatus a
compilerStatusFromMaybe _ (Just a) = return a
compilerStatusFromMaybe errorString _ = Error errorString "No Context"

dependencyRequired :: String -> a -> CompilerStatus a
dependencyRequired moduleName value = Valid (CC {moduleDependencies = singleton moduleName, nonFatalErrors = []}) value

addNonFatalError :: NonFatalError -> a -> CompilerStatus a
addNonFatalError error value = Valid (CC { moduleDependencies = Data.Set.empty, nonFatalErrors = [error]}) value

addNonFatalErrorM :: NonFatalError -> CompilerStatus a -> CompilerStatus a
addNonFatalErrorM _ (Error a b) = Error a b
addNonFatalErrorM error value = value >>= (addNonFatalError error)

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

newtype CompilerStatusT m a = CompilerStatusT { runCompilerStatusT :: m (CompilerStatus a) }

instance MonadTrans CompilerStatusT where
    lift = CompilerStatusT . fmap return

mapCompilerStatusT :: (m (CompilerStatus a) -> n (CompilerStatus b)) -> CompilerStatusT m a -> CompilerStatusT n b
mapCompilerStatusT f = CompilerStatusT . f . runCompilerStatusT

instance (Functor m) => Functor (CompilerStatusT m) where
    fmap f = mapCompilerStatusT (fmap (fmap f))

instance (Monad m) => Applicative (CompilerStatusT m) where
    pure = CompilerStatusT . return . return
    csfT <*> csaT = CompilerStatusT $ do
        csf <- runCompilerStatusT csfT
        csa <- runCompilerStatusT csaT
        return $ csf <*> csa

instance (Monad m) => Monad (CompilerStatusT m) where
    return = pure
    csaT >>= f = CompilerStatusT $ do
        csa <- runCompilerStatusT csaT
        csb <- case csa of
            PositionedError a b c -> return $ PositionedError a b c
            Error a b -> return $ Error a b
            Valid e a -> runCompilerStatusT $ f a
        return $ csa >> csb
