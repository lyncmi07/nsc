{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.If.Statement where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Blockable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Expression
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Data.List

type BlockStatement = Block Statement

data Statement = 
    SVarDec (SourcePosition VariableDeclaration)
    | SExpression (SourcePosition Expression)
    deriving Show

instance TargetCodeGeneratable Statement where
    generateD programEnvironment spStatement = case getContents spStatement of
        SVarDec varDec -> generateD programEnvironment varDec
        SExpression expr -> generateD programEnvironment expr
            
instance EnvironmentUpdater Statement where
    updateEnvironment programEnvironment spStatement  = case getContents spStatement of
        SVarDec varDec -> updateEnvironment programEnvironment varDec
        SExpression _ -> return programEnvironment
instance Blockable Statement where
    blockSeparator _ = ";\n"

instance {-# OVERLAPS #-} TargetCodeGeneratable BlockStatement where
    generateD programEnvironment spBlockStatement = case getContents spBlockStatement of
        SequentialBlock xs -> providePositionInfo spBlockStatement $ generateDForStatements programEnvironment xs

generateDForStatements programEnvironment@(PE { scopeReturnType = returnType }) (spStatement:[]) = case getContents spStatement of
    SVarDec a -> PositionedError (getSourcePosition spStatement) "The last statement in a block statement must be an expression" (show a)
    SExpression x -> do
        generatedExpression <- generateExpressionWithReturnType programEnvironment returnType x
        return $ "return " ++ generatedExpression ++ ";"
generateDForStatements programEnvironment (x:xs) = do
    generatedStatement <- generateD programEnvironment x
    updatedProgramEnvironment <- updateEnvironment programEnvironment x
    rest <- generateDForStatements updatedProgramEnvironment xs
    return $ generatedStatement ++ ";\n" ++ rest
generateDForStatements (PE { scopeReturnType = returnType }) [] =
    if returnType == "Nothing" then return "return;"
    else Error "Cannot use an empty block statment when return a value" ("Return type:" ++ returnType)
