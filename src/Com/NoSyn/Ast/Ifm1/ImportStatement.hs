{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.ImportStatement where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.ImportStatement as IfImportStatement
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionIfGenerator
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Listable

type ImportStatements = Block ImportStatement
data ImportStatement =
    IfImportStatement (SourcePosition IfImportStatement.ImportStatement)
    deriving Show

instance IfElementGeneratable ImportStatement where
    generateIfElement _ spImportStatement = case getContents spImportStatement of
        IfImportStatement a ->
            return $ changeContents spImportStatement $ IfElement.IfImportStatement a

instance IfElementGeneratable ImportStatements where
    generateIfElement programEnvironment spBlockImports = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) statementList
        ifImportStatements <- extractIfImportStatements ifElements
        return $ changeContents spBlockImports $ IfElement.IfImportStatements (changeContents (sequence ifImportStatements) (StandardBlock ifImportStatements))
        where
            statementList = toSourcePositionedList $ getContents spBlockImports

extractIfImportStatements :: [SourcePosition IfElement.IfElement] -> CompilerStatus [SourcePosition IfImportStatement.ImportStatement]
extractIfImportStatements [] = return []
extractIfImportStatements (spImportStatement:xs) = case getContents spImportStatement of
    IfElement.IfImportStatement x -> do
        xsm <- extractIfImportStatements xs
        return $ x:xsm
    otherwise -> PositionedError (getSourcePosition spImportStatement) "Unexpected non-import statement" (show $ getContents spImportStatement)
