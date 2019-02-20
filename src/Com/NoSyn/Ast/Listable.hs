{-# LANGUAGE KindSignatures #-}
module Com.NoSyn.Ast.Listable where

class Listable (m :: * -> *) where
    toList ::m a->[a]
