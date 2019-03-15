{-# LANGUAGE MultiParamTypeClasses #-}
module Com.NoSyn.Ast.Traits.IfCodeGeneratable where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus

class IfCodeGeneratable a b where
    generateIf :: (TargetCodeGeneratable b) => ProgramEnvironment -> a -> CompilerStatus b

