module Com.NoSyn.Ast.Traits.Blockable where

import Com.NoSyn.Ast.Traits.EnvironmentUpdater

class (EnvironmentUpdater m) => Blockable m where
    blockSeparator :: m -> String
