module Com.NoSyn.Ast.Typeable where

import Com.NoSyn.Error.CompilerStatus

class Typeable a where
    getTypeNoCheck :: a -> String
