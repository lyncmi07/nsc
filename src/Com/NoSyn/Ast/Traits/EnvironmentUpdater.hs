module Com.NoSyn.Ast.Traits.EnvironmentUpdater where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition

class EnvironmentUpdater a where
    updateEnvironment :: ProgramEnvironment -> SourcePosition a -> CompilerStatus ProgramEnvironment
