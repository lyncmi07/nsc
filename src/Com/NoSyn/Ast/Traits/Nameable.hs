module Com.NoSyn.Ast.Traits.Nameable where

class Nameable a where
    getName :: a -> String
    setName :: String -> a -> a
