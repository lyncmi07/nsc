module Com.NoSyn.Error.MaybeConvertable where

class MaybeConvertable m where
    toMaybe :: m a->Maybe a
