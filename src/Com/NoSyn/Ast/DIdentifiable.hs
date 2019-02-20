module Com.NoSyn.Ast.DIdentifiable where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus

class DIdentifiable a where
    getDIdentifier :: ProgramEnvironment -> a -> CompilerStatus String
