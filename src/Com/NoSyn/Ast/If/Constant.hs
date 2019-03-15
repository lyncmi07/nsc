{-# LANGUAGE MultiParamTypeClasses #-}
module Com.NoSyn.Ast.If.Constant where

import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.IfCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment

data Constant =
    CInt Int
    | CDouble Double
    | CChar Char

instance TargetCodeGeneratable Constant where
    generateD _ (CInt i) = return (show i)
    generateD _ (CDouble d) = return (show d)
    generateD _ (CChar c) = return (show c)
instance IfCodeGeneratable Constant Constant where
    generateIf programEnvironment constant = return constant

instance Typeable Constant where
    getTypeNoCheck (CInt _) = "Int"
    getTypeNoCheck (CDouble _) = "Double"
    getTypeNoCheck (CChar _) = "Char"

