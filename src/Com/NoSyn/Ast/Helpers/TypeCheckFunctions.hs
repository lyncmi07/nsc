module Com.NoSyn.Ast.Helpers.TypeCheckFunctions where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.Typeable

getRealType :: (Typeable a) => ProgramEnvironment -> a -> CompilerStatus String
getRealType (aliasEnvironment, _, _) a =
    lookupDType (getTypeNoCheck a) aliasEnvironment
getNoSynType :: (Typeable a) => ProgramEnvironment -> a -> CompilerStatus String
getNoSynType programEnvironment a = do
    _ <- getRealType programEnvironment a
    return $ getTypeNoCheck a
