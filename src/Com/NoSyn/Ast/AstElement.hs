module Com.NoSyn.Ast.AstElement where

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment

class AstElement a where
    generateD :: ProgramEnvironment -> a -> CompilerStatus String
