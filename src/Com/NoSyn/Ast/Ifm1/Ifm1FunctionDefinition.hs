{-#LANGUAGE MultiParamTypeClasses #-}
module Com.NoSyn.Ast.Ifm1.Ifm1FunctionDefinition where

import Com.NoSyn.Ast.If.FunctionDefinition
import Com.NoSyn.Ast.If.Statement
import Com.NoSyn.Ast.If.Parameter
import Com.NoSyn.Ast.Traits.IfCodeGeneratable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.Nameable

data OperatorType =
    Prefix
    | Postfix
    | Infix
    deriving Show

operatorNames = [
        ('+', "Plus"),
        ('-', "Minus"),
        ('/', "Slash"),
        ('*', "Asterisk"),
        ('?', "Question"),
        ('^', "Power"),
        ('#', "Hash"),
        ('@', "At"),
        (':', "Colon"),
        ('£', "Pound"),
        ('$', "Dollar"),
        ('%', "Percent"),
        ('&', "Ampersand"),
        ('|', "Bar"),
        ('!', "Exclaimation"),
        ('.', "Period"),
        ('<', "Less"),
        ('>', "Greater")]

type OperatorString = String

data Ifm1FunctionDefinition =
    IfFD FunctionDefinition
    | Ifm1OpOverload OperatorType OperatorString String Parameters BlockStatement

instance IfCodeGeneratable Ifm1FunctionDefinition FunctionDefinition where
    generateIf _ (IfFD funcDef) =
        let noSynName = getName funcDef in
            return $ setName (noSynName ++ "_function") funcDef
    generateIf _ (Ifm1OpOverload opType operatorString returnType params blockStmt) = do
        namedOperators <- compilerStatusFromMaybe (operatorString ++ " is an invalid operator") $ sequence $ map (\x -> lookup x operatorNames) operatorString
        let functionName = (show opType ++ "_" ++ (concat namedOperators) ++ "_operator") in
            return $ FDNoSyn functionName returnType params blockStmt
