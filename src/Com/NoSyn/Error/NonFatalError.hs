module Com.NoSyn.Error.NonFatalError where

type ErrorMessage = String
type RelevantCode = String
-- type StartLineNumber = Int
-- type EndLineNumber = Int
-- type StartColumn = Int
-- type EndColumn = Int

--NFE ErrorMessage RelevantCode StartLineNumber StartColumn EndLineNumber EndColumn
data NonFatalError = NFE ErrorMessage RelevantCode Int Int Int Int
    deriving (Show,Eq)
