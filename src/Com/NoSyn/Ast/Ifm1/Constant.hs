module Com.NoSyn.Ast.Ifm1.Constant where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Constant as IfConstant
import Com.NoSyn.Ast.Traits.IfElementGeneratable

data Constant =
    IfConstant IfConstant.Constant

instance IfElementGeneratable Constant where
    generateIfElement programEnvironment (IfConstant a) =
        return $ IfElement.IfConstant a
