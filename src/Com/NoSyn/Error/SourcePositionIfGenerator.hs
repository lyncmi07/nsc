module Com.NoSyn.Error.SourcePositionIfGenerator where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Error.SourcePosition

instance (IfElementGeneratable a) => IfElementGeneratable (SourcePosition a) where
    generateIfElement programEnvironment a = generateIfElement programEnvironment (getContents a)
