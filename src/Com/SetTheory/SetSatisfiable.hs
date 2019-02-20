module Com.SetTheory.SetSatisfiable where

class SetSatisfiable a where
    -- |Returns the union of the two objects
    union :: a -> a -> a
    -- |Returns the difference between the two objects
    difference :: a -> a -> a
    -- |The number of elements that are in the set
    size :: a -> Int

-- |Returns the union of two objects if and only if the intersection of the two objects if empty.
--Otherwise it returns the failure monad provided
concatenate :: (SetSatisfiable a, Monad m) => m a -> a -> a -> m a
concatenate failureMonad a b
    | size (intersection a b) == 0 = return $ union a b
    | otherwise = failureMonad

-- |Returns the intersection of the two objects
intersection :: (SetSatisfiable a) => a -> a -> a
intersection a b = difference a (difference a b)
