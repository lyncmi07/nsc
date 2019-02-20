module Com.NoSyn.Ast.Statement where

import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Ast.Block
import Com.NoSyn.Ast.VariableDeclaration
import Com.NoSyn.Ast.Expression
import Com.NoSyn.Ast.EnvironmentUpdater
import Data.List


type BlockStatement = Block Statement

data Statement = 
    SVarDec VariableDeclaration
    | SExpression Expression
instance AstElement Statement where
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
