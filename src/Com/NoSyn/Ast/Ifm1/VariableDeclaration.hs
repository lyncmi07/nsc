module Com.NoSyn.Ast.Ifm1.VariableDeclaration where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.VariableDeclaration as IfVariableDeclaration
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Error.SourcePosition

data VariableDeclaration =
    IfVariableDeclaration (SourcePosition IfVariableDeclaration.VariableDeclaration)
    deriving Show

instance IfElementGeneratable VariableDeclaration where
    generateIfElement _ spVarDec = case getContents spVarDec of
        IfVariableDeclaration a -> 
            return $ changeContents spVarDec $ IfElement.IfVariableDeclaration a
