module Com.NoSyn.Ast.If.PreProgram where

import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.If.ImportStatement
import Com.NoSyn.Ast.If.Program

data PreProgram = PreProgram ImportStatements Program
    deriving Show

instance TargetCodeGeneratable PreProgram where
    generateD programEnvironment (PreProgram imports program) = do
        importTargetCode <- generateD programEnvironment imports
        programTargetCode <- generateD programEnvironment program
        return $ importTargetCode ++ "\n" ++ programTargetCode

instance EnvironmentUpdater PreProgram where
    updateEnvironment programEnvironment (PreProgram _ program)= do
        updateEnvironment programEnvironment program
