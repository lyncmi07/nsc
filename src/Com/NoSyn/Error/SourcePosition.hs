module Com.NoSyn.Error.SourcePosition where

import Prelude hiding (getContents)

type LineNumber = Int
type Column = Int

data SourcePosition a = SourcePosition LineNumber Column LineNumber Column a

changeContents (SourcePosition a b c d e) f = (SourcePosition a b c d f)
getContents (SourcePosition _ _ _ _ a) = a

getSourcePosition (SourcePosition sl sc el ec _) = (sl, sc, el, ec)
setSourcePosition (SourcePosition _ _ _ _ a) (sl, sc, el, ec) = SourcePosition sl sc el ec a
combineSourcePositions sp1 sp2 =
    let (sl1, sc1, el1, ec1) = getSourcePosition sp1 in
    let (sl2, sc2, el2, ec2) = getSourcePosition sp1 in
    let (slc, scc) = min (sl1, sc1) (sl2, sc2) in
    let (elc, ecc) = min (el1, ec1) (el2, ec2) in
    (slc, scc, elc, ecc)

instance Functor SourcePosition where
    fmap f sp = changeContents sp (f $ Com.NoSyn.Error.SourcePosition.getContents sp)

instance Applicative SourcePosition where
    pure = SourcePosition 0 0 0 0
    spf <*> spa = setSourcePosition (pure $ (getContents spf) (getContents spa)) (combineSourcePositions spf spa)

instance Monad SourcePosition where
    -- Should the positions be combined here or do we just take the new positions?
    spa >>= f = f (getContents spa)
    return = pure
