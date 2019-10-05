{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.Program where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Program as IfProgram
import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.VariableDeclaration
import Com.NoSyn.Ast.Ifm1.AliasDefinition
import Com.NoSyn.Ast.Ifm1.FunctionDefinition
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Listable

type Program = Block ProgramStmt
data ProgramStmt =
    PSAliasDef (SourcePosition AliasDefinition)
    | PSFuncDef (SourcePosition FunctionDefinition)
    | PSVarDec (SourcePosition VariableDeclaration)
    deriving Show

instance IfElementGeneratable ProgramStmt where
    generateIfElement programEnvironment (PSAliasDef a) = do
        ~(IfElement.IfAliasDefinition b) <- generateIfElement programEnvironment $ getContents a
        return $ IfElement.IfProgramStmt (IfProgram.PSAliasDef $ changeContents a b)
    generateIfElement programEnvironment (PSVarDec a) = do
        ~(IfElement.IfVariableDeclaration b) <- generateIfElement programEnvironment $ getContents a
        return $ IfElement.IfProgramStmt (IfProgram.PSVarDec $ changeContents a b)
    generateIfElement programEnvironment (PSFuncDef a) = do
        ~(IfElement.IfFunctionDefinition b) <- generateIfElement programEnvironment $ getContents a
        return $ IfElement.IfProgramStmt (IfProgram.PSFuncDef $ changeContents a b)


instance IfElementGeneratable Program where
    generateIfElement programEnvironment program = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) programStmtList
        ifProgramStmts <- extractIfProgramStatements ifElements
        return $ IfElement.IfProgram (StandardBlock ifProgramStmts)
        where
            programStmtList = toList program

extractIfProgramStatements :: [IfElement.IfElement] -> CompilerStatus [IfProgram.ProgramStmt]
extractIfProgramStatements [] = return []
extractIfProgramStatements ((IfElement.IfProgramStmt x):xs) = do
    xsm <- extractIfProgramStatements xs
    return $ x:xsm
extractIfProgramStatements a = Error "Unexpected non-ProgramStatement" (show a)
