{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.Statement where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Statement as IfStatement
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.Expression
import Com.NoSyn.Ast.Ifm1.VariableDeclaration
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition

type BlockStatement = Block (SourcePosition Statement)
data Statement =
    SVarDec (SourcePosition VariableDeclaration)
    | SExpression (SourcePosition Expression)
    deriving Show

instance IfElementGeneratable Statement where
    generateIfElement programEnvironment (SVarDec a) = do
        ~(IfElement.IfVariableDeclaration b) <- generateIfElement programEnvironment a
        return $ IfElement.IfStatement (return $ IfStatement.SVarDec b)
    generateIfElement programEnvironment (SExpression a) = do
        ~(IfElement.IfExpression b) <- generateIfElement programEnvironment a
        return $ IfElement.IfStatement (return $ IfStatement.SExpression b)

instance IfElementGeneratable BlockStatement where
    generateIfElement programEnvironment blockStatements = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) statementList
        ifStatements <- extractIfStatements (map return ifElements)
        return $ IfElement.IfBlockStatement (return $ SequentialBlock ifStatements)
        where
            statementList = toList blockStatements

extractIfStatements :: [SourcePosition IfElement.IfElement] -> CompilerStatus [SourcePosition IfStatement.Statement]
extractIfStatements [] = return []
extractIfStatements (spX:xs) = case getContents spX of
    IfElement.IfStatement x -> do
        xsm <- extractIfStatements xs
        return $ x:xsm
extractIfStatements a = Error "Unexpected non-statement" (show a)
