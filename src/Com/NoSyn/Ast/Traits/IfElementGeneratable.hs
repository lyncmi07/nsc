module Com.NoSyn.Ast.Traits.IfElementGeneratable where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.If.IfElement
import Com.NoSyn.Error.CompilerStatus

class IfElementGeneratable a where
    generateIfElement :: ProgramEnvironment -> a -> CompilerStatus IfElement
