module Com.NoSyn.Ast.If.Constant where

import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment

data Constant =
    CInt Int
    | CDouble Double
    | CChar Char
    deriving Show

instance TargetCodeGeneratable Constant where
    generateD _ (CInt i) = return (show i)
    generateD _ (CDouble d) = return (show d)
    generateD _ (CChar c) = return (show c)



instance Typeable Constant where
    getTypeNoCheck (CInt _) = "Int"
    getTypeNoCheck (CDouble _) = "Double"
    getTypeNoCheck (CChar _) = "Char"

