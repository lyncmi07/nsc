module Com.NoSyn.Ast.Ifm1.PreProgram where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.PreProgram as IfPreProgram
import Com.NoSyn.Ast.Traits.IfElementGeneratable

data PreProgram = IfPreProgram IfPreProgram.PreProgram
    deriving Show

instance IfElementGeneratable PreProgram where
    generateIfElement _ (IfPreProgram a) = 
        return $ IfElement.IfPreProgram a
