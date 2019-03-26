module Com.NoSyn.Ast.Ifm1.Program where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Program as IfProgram
import Com.NoSyn.Ast.Traits.IfElementGeneratable

data Program =
    IfProgram IfProgram.Program

instance IfElementGeneratable Program where
    generateIfElement _ (IfProgram a) =
        return $ IfElement.IfProgram a
