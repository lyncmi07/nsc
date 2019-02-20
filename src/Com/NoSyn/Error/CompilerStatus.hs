module Com.NoSyn.Error.CompilerStatus where

import Com.NoSyn.Error.MaybeConvertable

data CompilerStatus a =
    Valid a
    | Error String
    deriving (Show, Eq)

instance Functor CompilerStatus where
    fmap f (Valid a) = return (f a)
    fmap f (Error a) = Error a

instance Applicative CompilerStatus where
    pure = Valid
    (Error a) <*> _ = Error a
    (Valid a) <*> something = fmap a something

instance Monad CompilerStatus where
    (Valid a) >>= f = f a
    (Error a) >>= f = Error a
    return = Valid

instance MaybeConvertable CompilerStatus where
    toMaybe (Valid a) = (Just a)
    toMaybe (Error _) = Nothing

compilerStatusFromMaybe::String->Maybe a->CompilerStatus a
compilerStatusFromMaybe _ (Just a) = Valid a
compilerStatusFromMaybe errorString _ = Error errorString
