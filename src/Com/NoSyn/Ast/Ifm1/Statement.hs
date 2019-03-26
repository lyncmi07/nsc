module Com.NoSyn.Ast.Ifm1.Statement where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Statement as IfStatement
import Com.NoSyn.Ast.Traits.IfElementGeneratable

data Statement =
    IfStatement IfStatement.Statement

instance IfElementGeneratable Statement where
    generateIfElement _ (IfStatement a) =
        return $ IfElement.IfStatement a
