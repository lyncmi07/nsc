module Com.NoSyn.Error.NonFatalError where

type ErrorMessage = String
type RelevantCode = String

data NonFatalError = NFE ErrorMessage RelevantCode
    deriving (Show,Eq)
