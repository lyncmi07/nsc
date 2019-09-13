module Com.NoSyn.Error.NonFatalError where

type ErrorMessage = String
type RelevantCode = String
-- type LineNumber = Int

data NonFatalError = NFE ErrorMessage RelevantCode Int
    deriving (Show,Eq)
