{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.If.Statement where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Expression
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Data.List

type BlockStatement = Block Statement

data Statement = 
    SVarDec VariableDeclaration
    | SExpression Expression
    deriving Show

instance TargetCodeGeneratable Statement where
    generateD programEnvironment (SVarDec varDec) =
        generateD programEnvironment varDec
    generateD programEnvironment (SExpression expr) =
        generateD programEnvironment expr

instance EnvironmentUpdater Statement where
    updateEnvironment programEnvironment (SVarDec varDec) =
        updateEnvironment programEnvironment varDec
    updateEnvironment programEnvironment (SExpression _) = return programEnvironment
instance Blockable Statement where
    blockSeparator _ = ";\n"

instance {-# OVERLAPS #-} TargetCodeGeneratable BlockStatement where
    generateD programEnvironment (SequentialBlock xs) = generateDForStatements programEnvironment xs

generateDForStatements _ ((SVarDec a):[]) = Error "The last statement in a block statement must be an expression" (show a)
generateDForStatements programEnvironment@(PE { scopeReturnType = returnType }) (SExpression x:[]) = do
    generatedExpression <- generateExpressionWithReturnType programEnvironment returnType x
    return $ "return " ++ generatedExpression ++ ";"
generateDForStatements programEnvironment (x:xs) = do
    generatedStatement <- generateD programEnvironment x
    updatedProgramEnvironment <- updateEnvironment programEnvironment x
    rest <- generateDForStatements updatedProgramEnvironment xs
    return $ generatedStatement ++ ";\n" ++ rest
