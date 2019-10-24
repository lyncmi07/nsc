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
    generateIfElement programEnvironment spProgramStmt = case getContents spProgramStmt of
        PSAliasDef a -> do
            positionedAliasDef <- generateIfElement programEnvironment a
            let ~(IfElement.IfAliasDefinition b) = getContents positionedAliasDef
            return $ changeContents spProgramStmt $ IfElement.IfProgramStmt $ changeContents a (IfProgram.PSAliasDef b)
        PSVarDec a -> do
            positionedVarDec <- generateIfElement programEnvironment a
            let ~(IfElement.IfVariableDeclaration b) = getContents positionedVarDec
            return $ changeContents spProgramStmt $ IfElement.IfProgramStmt $ changeContents a (IfProgram.PSVarDec b)
        PSFuncDef a -> do
            positionedFunctionDef <- generateIfElement programEnvironment a
            let ~(IfElement.IfFunctionDefinition b) = getContents positionedFunctionDef
            return $ changeContents spProgramStmt $ IfElement.IfProgramStmt $ changeContents a (IfProgram.PSFuncDef b)

instance IfElementGeneratable Program where
    generateIfElement programEnvironment spProgram = do
        ifElements <- sequence $ map (generateIfElement programEnvironment) programStmtList
        ifProgramStmts <- extractIfProgramStatements ifElements
        return $ changeContents spProgram $ IfElement.IfProgram $ return $ (StandardBlock ifProgramStmts)
        where
            programStmtList = toSourcePositionedList $ getContents spProgram

extractIfProgramStatements :: [SourcePosition IfElement.IfElement] -> CompilerStatus [SourcePosition IfProgram.ProgramStmt]
extractIfProgramStatements [] = return []
extractIfProgramStatements (spProgramStmt:xs) = case getContents spProgramStmt of
    IfElement.IfProgramStmt x -> do
        xsm <- extractIfProgramStatements xs
        return $ x:xsm
    otherwise -> PositionedError (getSourcePosition spProgramStmt) "Unexpected non-ProgramStatement" (show $ getContents spProgramStmt)
