module Com.NoSyn.Ast.Ifm1.PreProgram where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.PreProgram as IfPreProgram
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.ImportStatement
import Com.NoSyn.Ast.Ifm1.Program

data PreProgram = PreProgram ImportStatements Program
    deriving Show

instance IfElementGeneratable PreProgram where
    generateIfElement programEnvironment (PreProgram a b) = do
        ~(IfElement.IfImportStatements importStatements) <- generateIfElement programEnvironment a
        ~(IfElement.IfProgram program) <- generateIfElement programEnvironment b
        return $ IfElement.IfPreProgram $ IfPreProgram.PreProgram importStatements program
