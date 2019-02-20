module Com.NoSyn.Data.Variable where

import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.Typeable

data Variable = 
    VConst Ident Ident
    | VPointer Ident Ident
    deriving Show

instance Typeable Variable where
    getTypeNoCheck (VConst varType _) = varType
    getTypeNoCheck (VPointer varType _) = varType

generateDPointerReference::Variable->String
generateDPointerReference (VConst _ varName) = "&" ++ varName
generateDPointerReference (VPointer _ varName) = varName

generateDStandardReference::Variable->String
generateDStandardReference (VConst _ varName) = varName
generateDStandardReference (VPointer _ varName) = "(*" ++ varName ++ ")"
