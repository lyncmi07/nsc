module Com.NoSyn.Ast.Traits.Typeable where

import Com.NoSyn.Error.CompilerStatus

class Typeable a where
    getTypeNoCheck :: a -> String
    getAlphaTypeName :: a -> String
