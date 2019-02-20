module Com.NoSyn.Ast.Constant where

import Com.NoSyn.Ast.Typeable
import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Environment.ProgramEnvironment

data Constant =
    CInt Int
    | CDouble Double
    | CChar Char

instance AstElement Constant where
    generateD _ (CInt i) = return (show i)
    generateD _ (CDouble d) = return (show d)
    generateD _ (CChar c) = return (show c)
instance Typeable Constant where
    getTypeNoCheck (CInt _) = "Int"
    getTypeNoCheck (CDouble _) = "Double"
    getTypeNoCheck (CChar _) = "Char"

