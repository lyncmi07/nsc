module Com.NoSyn.Ast.EnvironmentUpdater where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus

class EnvironmentUpdater a where
    updateEnvironment :: ProgramEnvironment -> a -> CompilerStatus ProgramEnvironment
