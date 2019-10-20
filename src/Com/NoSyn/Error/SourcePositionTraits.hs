module Com.NoSyn.Error.SourcePositionTraits where

import Prelude hiding (getContents)
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.Traits.Blockable

instance (TargetCodeGeneratable a) => TargetCodeGeneratable (SourcePosition a) where
    generateD programEnvironment a = generateD programEnvironment (getContents a)

instance (EnvironmentUpdater a) => EnvironmentUpdater (SourcePosition a) where
    updateEnvironment programEnvironment a = updateEnvironment programEnvironment (getContents a)

instance (Blockable a) => Blockable (SourcePosition a) where
    blockSeparator a = blockSeparator $ getContents a

instance (Typeable a) => Typeable (SourcePosition a) where
    getTypeNoCheck a = getTypeNoCheck (getContents a)
    getAlphaTypeName aliasEnvironment a = getAlphaTypeName aliasEnvironment (getContents a)

sourcePositionToList :: Listable m => SourcePosition (m a) -> [a]
sourcePositionToList a = toList $ getContents a
