module Com.NoSyn.Error.NonFatalError where

type ErrorMessage = String
type RelevantCode = String
-- type LineNumber = Int
-- type StartColumn = Int
-- type EndColumn = Int

--NFE ErrorMessage RelevantCode LineNumber StartColumn EndColumn
data NonFatalError = NFE ErrorMessage RelevantCode Int Int Int
    deriving (Show,Eq)
