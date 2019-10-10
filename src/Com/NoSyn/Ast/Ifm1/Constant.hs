module Com.NoSyn.Ast.Ifm1.Constant where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Constant as IfConstant
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Error.SourcePosition

data Constant =
    IfConstant (SourcePosition IfConstant.Constant)
    deriving Show

instance IfElementGeneratable Constant where
    generateIfElement _ (IfConstant a) =
        return $ IfElement.IfConstant a
