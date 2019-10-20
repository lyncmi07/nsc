module Com.NoSyn.Ast.If.PreProgram where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.If.ImportStatement
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Error.SourcePosition

data PreProgram = PreProgram (SourcePosition ImportStatements) (SourcePosition Program)
    deriving Show

instance TargetCodeGeneratable PreProgram where
    generateD programEnvironment spPreProgram = case getContents spPreProgram of
        PreProgram imports program -> do
            importTargetCode <- generateD programEnvironment imports
            programTargetCode <- generateD programEnvironment program
            return $ importTargetCode ++ ";\n" ++ programTargetCode

instance EnvironmentUpdater PreProgram where
    updateEnvironment programEnvironment spPreProgram = case getContents spPreProgram of
        PreProgram _ program ->
            updateEnvironment programEnvironment program
