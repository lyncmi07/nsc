module Com.NoSyn.Ast.Ifm1.Parameter where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.Traits.IfElementGeneratable

data Parameter = 
    IfParameter IfParameter.Parameter

instance IfElementGeneratable Parameter where
    generateIfElement programEnvironment (IfParameter a) =
        return $ IfElement.IfParameter a
