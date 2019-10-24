module Com.NoSyn.Ast.If.Constant where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.SourcePosition

data Constant =
    CInt Int
    | CDouble Double
    | CChar Char
    | CString String
    deriving Show

instance TargetCodeGeneratable Constant where
    generateD _ spConstant = case getContents spConstant of
        CInt i -> return (show i)
        CDouble d -> return (show d)
        CChar c -> return (show c)
        CString s -> return (show s)

instance Typeable Constant where
    getTypeNoCheck (CInt _) = "Int"
    getTypeNoCheck (CDouble _) = "Double"
    getTypeNoCheck (CChar _) = "Char"
    getTypeNoCheck (CString _) = "String"
    getAlphaTypeName _ x = return $ getTypeNoCheck x

