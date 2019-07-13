module Com.NoSyn.Ast.If.Constant where

import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment

data Constant =
    CInt Int
    | CDouble Double
    | CChar Char
    | CString String
    deriving Show

instance TargetCodeGeneratable Constant where
    generateD _ (CInt i) = return (show i)
    generateD _ (CDouble d) = return (show d)
    generateD _ (CChar c) = return (show c)
    generateD _ (CString s) = return (show s)



instance Typeable Constant where
    getTypeNoCheck (CInt _) = "Int"
    getTypeNoCheck (CDouble _) = "Double"
    getTypeNoCheck (CChar _) = "Char"
    getTypeNoCheck (CString _) = "String"

