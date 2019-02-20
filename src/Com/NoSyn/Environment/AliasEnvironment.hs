module Com.NoSyn.Environment.AliasEnvironment where

import Data.Map.Ordered
import Com.NoSyn.Data.Types
import Com.NoSyn.Error.CompilerStatus

emptyAliasEnvironment = empty
defaultAliasEnvironment = fromList dTypes
type AliasEnvironment = OMap Ident Ident 


