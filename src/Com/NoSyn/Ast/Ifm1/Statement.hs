{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.Statement where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Statement as IfStatement
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.Expression
import Com.NoSyn.Ast.Ifm1.VariableDeclaration
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Error.CompilerStatus

type BlockStatement = Block Statement
data Statement =
    SVarDec VariableDeclaration
    | SExpression Expression
    deriving Show

instance IfElementGeneratable Statement where
    generateIfElement programEnvironment (SVarDec a) = do
        ~(IfElement.IfVariableDeclaration b) <- generateIfElement programEnvironment a
        return $ IfElement.IfStatement (IfStatement.SVarDec b)
    generateIfElement programEnvironment (SExpression a) = do
        ~(IfElement.IfExpression b) <- generateIfElement programEnvironment a
        return $ IfElement.IfStatement (IfStatement.SExpression b)

instance IfElementGeneratable BlockStatement where
    generateIfElement programEnvironment blockStatements = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) statementList
        ifStatements <- extractIfStatements ifElements
        return $ IfElement.IfBlockStatement (SequentialBlock ifStatements)
        where
            statementList = toList blockStatements

extractIfStatements :: [IfElement.IfElement] -> CompilerStatus [IfStatement.Statement]
extractIfStatements [] = return []
extractIfStatements ((IfElement.IfStatement x):xs) = do
    xsm <- extractIfStatements xs
    return $ x:xsm
extractIfStatements a = Error "Unexpected non-statement" (show a)
