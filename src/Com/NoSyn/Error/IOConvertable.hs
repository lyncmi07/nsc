module Com.NoSyn.Error.IOConvertable where

class IOConvertable m where
    toIO :: m a->IO a
