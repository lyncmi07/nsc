module Com.HigherOrderFunctions where

-- |Performs a map operation on a list of objects while updating an environment variable on those same objects.
-- This environment is used as the environment in the next object in the list.
environmentUpdatingMap::(a -> b -> a) -> (a -> b -> c) -> a -> [b] -> [c]
environmentUpdatingMap _ _ _ [] = []
environmentUpdatingMap environmentUpdater transformer environment (x:xs) =
    let updatedEnvironment = environmentUpdater environment x in
        (transformer environment x):(environmentUpdatingMap environmentUpdater transformer updatedEnvironment xs)

-- |This is similar to 'environmentUpdatingMap' but provides monad support to the updating and map functions
-- This will return the transformed list within the given monad.
environmentUpdatingMapMonad::(Monad m) => (a -> b -> m a) -> (a -> b -> m c) -> a -> [b] -> m [c]
environmentUpdatingMapMonad _ _ _ [] = return []
environmentUpdatingMapMonad environmentUpdater transformer environment (x:xs) = do
    cVal <- transformer environment x
    updatedEnvironment <- environmentUpdater environment x
    rest <- environmentUpdatingMapMonad environmentUpdater transformer updatedEnvironment xs
    return $ cVal:rest
