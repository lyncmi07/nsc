{-# LANGUAGE KindSignatures #-}
module Com.NoSyn.Ast.Traits.Listable where

class Listable (m :: * -> *) where
    toList ::m a->[a]
