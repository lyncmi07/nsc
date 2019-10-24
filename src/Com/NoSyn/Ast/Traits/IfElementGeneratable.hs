module Com.NoSyn.Ast.Traits.IfElementGeneratable where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.If.IfElement
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition

class IfElementGeneratable a where
    generateIfElement :: ProgramEnvironment -> SourcePosition a -> CompilerStatus (SourcePosition IfElement)
