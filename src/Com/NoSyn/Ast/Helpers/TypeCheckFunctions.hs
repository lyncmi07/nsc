module Com.NoSyn.Ast.Helpers.TypeCheckFunctions where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.Typeable

getRealType :: (Typeable a) => ProgramEnvironment -> a -> CompilerStatus String
getRealType (PE { aliases = aliasEnvironment}) a =
    lookupDType (getTypeNoCheck a) aliasEnvironment

--once closed aliases are available this function will probably have to return the original closed alias name
getNoSynType :: (Typeable a) => ProgramEnvironment -> a -> CompilerStatus String
getNoSynType (PE { aliases = aliasEnvironment}) a = do
    atomicNoSynType <- lookupAtomicNoSynType (getTypeNoCheck a) aliasEnvironment
    return $ atomicNoSynType
