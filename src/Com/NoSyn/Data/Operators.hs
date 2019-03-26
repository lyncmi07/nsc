module Com.NoSyn.Data.Operators where

import Com.NoSyn.Error.CompilerStatus

data OperatorType =
    Prefix
    | Postfix
    | Infix
    deriving Show

data BracketType =
    Parentheses
    | Square
    | Curly
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

operatorStringConverter operatorString =
    compilerStatusFromMaybe (operatorString ++ " is an invalid operator") $ sequence $ map (\x -> lookup x operatorNames) operatorString


