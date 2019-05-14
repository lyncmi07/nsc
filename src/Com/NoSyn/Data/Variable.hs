module Com.NoSyn.Data.Variable where

import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.Nameable

data Variable = 
    VConst Ident Ident
    | VPointer Ident Ident
    deriving Show

instance Typeable Variable where
    getTypeNoCheck (VConst varType _) = varType
    getTypeNoCheck (VPointer varType _) = varType

instance Nameable Variable where
    getName (VConst _ name) = name
    getName (VPointer _ name) = name
    setName y (VConst x _) = VConst x y
    setName y (VPointer x _) = VPointer x y

generateDPointerReference::Variable->String
generateDPointerReference (VConst _ varName) = "&" ++ varName
generateDPointerReference (VPointer _ varName) = varName

generateDStandardReference::Variable->String
generateDStandardReference (VConst _ varName) = varName
generateDStandardReference (VPointer _ varName) = "(*" ++ varName ++ ")"
