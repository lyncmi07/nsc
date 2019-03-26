module Com.NoSyn.Ast.If.Statement where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.If.Expression
import Data.List

type BlockStatement = Block Statement

data Statement = 
    SVarDec VariableDeclaration
    | SExpression Expression
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
