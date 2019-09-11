module Com.NoSyn.Error.NonFatalError where

data NonFatalError = NFE String String
    deriving (Show,Eq)
