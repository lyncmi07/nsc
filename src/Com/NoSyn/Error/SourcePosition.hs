module Com.NoSyn.Error.SourcePosition where

import Prelude hiding (getContents)
import Control.Monad.Trans.Class
import Control.Monad (liftM)

type LineNumber = Int
type Column = Int

data SourcePosition a = 
    SourcePosition LineNumber Column LineNumber Column a
    | NoPosition a
    deriving (Show, Eq)

changeContents (SourcePosition a b c d e) f = SourcePosition a b c d f
changeContents (NoPosition a) b = NoPosition b
getContents (SourcePosition _ _ _ _ a) = a
getContents (NoPosition a) = a

headContents (SourcePosition a b c d e) (SourcePosition aT bT cT dT eT) f = 
    SourcePosition a b cT (if dT > 0 then dT - 1 else 0) f

getSourcePosition (SourcePosition sl sc el ec _) = (sl, sc, el, ec)
-- setSourcePosition (SourcePosition _ _ _ _ a) (sl, sc, el, ec) = SourcePosition sl sc el ec a
-- combineSourcePositions sp1 sp2 =
    -- let (sl1, sc1, el1, ec1) = getSourcePosition sp1 in
    -- let (sl2, sc2, el2, ec2) = getSourcePosition sp1 in
    -- let (slc, scc) = min (sl1, sc1) (sl2, sc2) in
    -- let (elc, ecc) = min (el1, ec1) (el2, ec2) in
    -- (slc, scc, elc, ecc)

combineSourcePositions :: SourcePosition a -> SourcePosition b -> SourcePosition (a, b)
combineSourcePositions (NoPosition a) (NoPosition b) = NoPosition (a, b)
combineSourcePositions (NoPosition a) spb@(SourcePosition _ _ _ _ b) =
    changeContents spb (a, b)
combineSourcePositions spa@(SourcePosition _ _ _ _ a) (NoPosition b) =
    changeContents spa (a, b)
combineSourcePositions spa spb = 
    let (sl1, sc1, el1, ec1) = getSourcePosition spa in
    let (sl2, sc2, el2, ec2) = getSourcePosition spb in
    let (slc, scc) = min (sl1, sc1) (sl2, sc2) in
    let (elc, ecc) = max (el1, ec1) (el2, ec2) in
    SourcePosition slc scc elc ecc ((getContents spa), (getContents spb))

instance Functor SourcePosition where
    fmap f sp = changeContents sp (f $ Com.NoSyn.Error.SourcePosition.getContents sp)

instance Applicative SourcePosition where
    pure = NoPosition
    spf <*> spa = changeContents (combineSourcePositions spf spa) ((getContents spf) (getContents spa))

instance Monad SourcePosition where
    -- Should the positions be combined here or do we just take the new positions?
    spa >>= f = let spb = f (getContents spa) in
        changeContents (combineSourcePositions spa spb) (getContents spb)
    return = pure

newtype SourcePositionT m a = SourcePositionT { runSourcePositionT :: m (SourcePosition a) }

instance MonadTrans SourcePositionT where
    lift = SourcePositionT . fmap return

mapSourcePositionT :: (m (SourcePosition a) -> n (SourcePosition b))  -> SourcePositionT m a -> SourcePositionT n b
mapSourcePositionT f = SourcePositionT . f . runSourcePositionT

instance (Functor m) => Functor (SourcePositionT m) where
    fmap f = mapSourcePositionT (fmap (fmap f))

instance (Monad m) => Applicative (SourcePositionT m) where
    pure = SourcePositionT . return . return
    spfT <*> spaT = SourcePositionT $ do
        spf <- runSourcePositionT spfT
        spa <- runSourcePositionT spaT
        return $ spf <*> spa

instance (Monad m) => Monad (SourcePositionT m) where
    return = SourcePositionT . return . return
    spaT >>= f = SourcePositionT $ do
        spa <- runSourcePositionT spaT
        spb <- runSourcePositionT $ (f (getContents spa))
        return $ spa >> spb
