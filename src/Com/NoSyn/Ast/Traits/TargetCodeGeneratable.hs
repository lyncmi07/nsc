module Com.NoSyn.Ast.Traits.TargetCodeGeneratable where

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment

class TargetCodeGeneratable a where
    generateD :: ProgramEnvironment -> a -> CompilerStatus String
