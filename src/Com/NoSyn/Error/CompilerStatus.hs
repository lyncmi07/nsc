module Com.NoSyn.Error.CompilerStatus where

import Com.NoSyn.Error.MaybeConvertable
import Com.NoSyn.Error.IOConvertable
import Com.NoSyn.Error.CompilerContext
import Data.Set.SetTheory
import Data.Set (empty, singleton, toList)

data CompilerStatus a =
    Valid CompilerContext a
    | Error String
    deriving (Show, Eq)

instance Functor CompilerStatus where
    fmap f (Valid cc a) = Valid cc (f a)
    fmap f (Error a) = Error a

instance Applicative CompilerStatus where
    pure = Valid (CC {moduleDependencies = empty})
    (Error a) <*> _ = Error a
    (Valid cc f) <*> a = case fmap f a of
        Valid ccv b -> Valid (union ccv cc) b
        Error errorMessage -> Error errorMessage

instance Monad CompilerStatus where
    (Valid cc a) >>= f = case (f a) of
        (Valid ccv b) -> Valid (union ccv cc) b
        (Error errorMessage) -> Error errorMessage
    (Error a) >>= f = Error a
    return = Valid (CC {moduleDependencies = empty})

instance MaybeConvertable CompilerStatus where
    toMaybe (Valid _ a) = (Just a)
    toMaybe (Error _) = Nothing

convertToIO :: CompilerStatus a -> IO ([String], a)
convertToIO (Valid compilerContext value) = return (toList (moduleDependencies compilerContext), value)
convertToIO (Error errorMessage) = fail errorMessage

compilerStatusFromMaybe::String->Maybe a->CompilerStatus a
compilerStatusFromMaybe _ (Just a) = return a
compilerStatusFromMaybe errorString _ = Error errorString

dependencyRequired :: String -> a -> CompilerStatus a
dependencyRequired moduleName value = Valid (CC {moduleDependencies = singleton moduleName}) value
