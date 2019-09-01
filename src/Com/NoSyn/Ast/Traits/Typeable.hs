module Com.NoSyn.Ast.Traits.Typeable where

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.AliasEnvironment

class Typeable a where
    getTypeNoCheck :: a -> String
    getAlphaTypeName :: AliasEnvironment -> a -> CompilerStatus String
