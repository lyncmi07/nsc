{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.ImportStatement where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.ImportStatement as IfImportStatement
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionIfGenerator
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Listable

type ImportStatements = Block (SourcePosition ImportStatement)
data ImportStatement =
    IfImportStatement (SourcePosition IfImportStatement.ImportStatement)
    deriving Show

instance IfElementGeneratable ImportStatement where
    generateIfElement _ (IfImportStatement a) =
        return $ IfElement.IfImportStatement a

instance IfElementGeneratable ImportStatements where
    generateIfElement programEnvironment blockImports = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) statementList
        ifImportStatements <- extractIfImportStatements ifElements
        return $ IfElement.IfImportStatements (changeContents (sequence ifImportStatements) (StandardBlock ifImportStatements))
        where
            statementList = toList blockImports

extractIfImportStatements :: [IfElement.IfElement] -> CompilerStatus [SourcePosition IfImportStatement.ImportStatement]
extractIfImportStatements [] = return []
extractIfImportStatements ((IfElement.IfImportStatement x):xs) = do
    xsm <- extractIfImportStatements xs
    return $ x:xsm
extractIfImportStatements a = Error "Unexpected non-import statement" (show a)
