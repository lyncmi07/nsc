module Com.NoSyn.Ast.Ifm1.PreProgram where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.PreProgram as IfPreProgram
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.ImportStatement
import Com.NoSyn.Ast.Ifm1.Program
import Com.NoSyn.Error.SourcePosition

data PreProgram = PreProgram (SourcePosition ImportStatements) (SourcePosition Program)
    deriving Show

instance IfElementGeneratable PreProgram where
    generateIfElement programEnvironment spPreProgram = case getContents spPreProgram of
        PreProgram a b -> do
            positionedImportStatements <- generateIfElement programEnvironment a
            positionedProgram <- generateIfElement programEnvironment b
            let ~(IfElement.IfImportStatements importStatements) = getContents positionedImportStatements
            let ~(IfElement.IfProgram program) = getContents positionedProgram
            return $ changeContents spPreProgram $ IfElement.IfPreProgram $ changeContents spPreProgram $ IfPreProgram.PreProgram importStatements program
