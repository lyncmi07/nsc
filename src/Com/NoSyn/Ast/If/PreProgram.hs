module Com.NoSyn.Ast.If.PreProgram where

data PreProgram = PreProgram ImportStatements Program
    deriving Show

instance TargetCodeGeneratable PreProgram where
    generateD programEnvironment (PreProgram imports program) = do
        importTargetCode <- generateD programEnvironment imports
        programTargetCode <- generateD programEnvironment program

instance EnvironmentUpdater PreProgram where
    updateEnvironment programEnvironment (PreProgram _ program)= do
        updateEnvironment programEnvironment program
