{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Trans.Skeleton
  (
    -- transformer api
    MonadView(..)
  , hoistMV
  , iterMV
  , SkeletonT(..)
  , boneT
  , deboneT
  , unboneT
  , bonedT
  , Spine(..)
  , SkeletonViewT
  , iterT
  , iterTM
    -- aliases
  , debone
  , unbone
  , Skeleton
  , SkeletonView
  , iter
  , iterM
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Monad                   (MonadPlus (..), ap, join,
                                                  liftM)
import           Control.Monad.Catch             (MonadCatch (..),
                                                  MonadThrow (..))
import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Skeleton.Internal
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Class
import           Prelude                         hiding (id, (.))
import           Unsafe.Coerce

type SkeletonView t a = SkeletonViewT t Identity a
type Skeleton t a = SkeletonT t Identity a

type SkeletonViewT t m a = MonadView t (SkeletonT t m) a

-- | Re-add a bone.
bonedT :: (Monad m) => SkeletonViewT t m a -> SkeletonT t m a
bonedT t = SkeletonT $ return $ Spine t id
{-# INLINE bonedT #-}

-- | Pick a bone from a 'SkeletonT'.
deboneT :: (Monad m) => SkeletonT t m a -> m (SkeletonViewT t m a)
deboneT (SkeletonT x) = do
  Spine xv s <- x
  case xv of
    Return a -> viewL s (return $ Return a) $ \(Kleisli k) c -> case k a of
      SkeletonT xt -> deboneT $ SkeletonT $ (<$> xt) $ \(Spine h t) -> Spine h (c . t)
    t :>>= k -> return $ t :>>= \a -> case k a of
      SkeletonT xt -> SkeletonT $ (<$> xt) $ \(Spine h c) -> Spine h (s . c)

-- | Uncommon synonym for 'deboneT'.
unboneT :: (Monad m) => SkeletonT t m a -> m (SkeletonViewT t m a)
unboneT = deboneT
{-# INLINE unboneT #-}

-- | Pick a bone from a 'Skeleton'.
debone :: Skeleton t a -> SkeletonView t a
debone = runIdentity . deboneT
{-# INLINE debone #-}

-- | Uncommon synonym for 'debone'.
unbone :: Skeleton t a -> SkeletonView t a
unbone = debone
{-# INLINE unbone #-}

-- | A skeleton that has only one bone.
boneT :: (Monad m) => t a -> SkeletonT t m a
boneT t = SkeletonT $ return $ Spine (t :>>= return) id
{-# INLINABLE boneT #-}

data MonadView t m x where
  Return :: a -> MonadView t m a
  (:>>=) :: t a -> (a -> m b) -> MonadView t m b
infixl 1 :>>=

instance Functor m => Functor (MonadView t m) where
  fmap f (Return a) = Return (f a)
  fmap f (t :>>= k) = t :>>= fmap f . k
  {-# INLINE fmap #-}

hoistMV :: (forall x. s x -> t x) -> (m a -> n a) -> MonadView s m a -> MonadView t n a
hoistMV _ _ (Return a) = Return a
hoistMV f g (t :>>= k) = f t :>>= g . k
{-# INLINE hoistMV #-}

iterMV :: Monad m => (t a -> MonadView m t a) -> t a -> m a
iterMV f = go where
  go t = case f t of
    m :>>= k -> m >>= go . k
    Return a -> return a
{-# INLINE iterMV #-}

iterT :: Monad m => (SkeletonViewT t m a -> m a) -> SkeletonT t m a -> m a
iterT f p = deboneT p >>= f

iterTM :: (Monad m, MonadTrans t, Monad (t m)) =>
  (SkeletonViewT s m a -> t m a) -> SkeletonT s m a -> t m a
iterTM f p = lift (deboneT p) >>= f

iter :: (SkeletonView t a -> a) -> Skeleton t a -> a
iter f = f . runIdentity . deboneT

iterM :: Monad m => (SkeletonView s a -> m a) -> Skeleton s a -> m a
iterM f = f . runIdentity . deboneT

data Spine t m a where
  Spine :: !(MonadView t m a) -> !(Cat (Kleisli m) a b) -> Spine t m b

newtype SkeletonT t m a = SkeletonT { unSkeletonT :: m (Spine t (SkeletonT t m) a) }

instance Monad m => Functor (SkeletonT t m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Monad m => Applicative (SkeletonT t m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (SkeletonT t m) where
  return a = SkeletonT $ return $ Spine (Return a) id
  {-# INLINE return #-}
  SkeletonT x >>= k = SkeletonT $ do
    Spine t c <- x
    return $ Spine t (c |> Kleisli k)
  {-# INLINE (>>=) #-}

instance MonadTrans (SkeletonT t) where
  lift x = SkeletonT $ (\a -> Spine (Return a) id) <$> x
  {-# INLINE lift #-}

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f = unsafeCoerce (f.)
{-# INLINE transKleisli #-}

instance (MonadIO m) => MonadIO (SkeletonT t m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

--instance (MonadReader r m) => MonadReader r (SkeletonT t m) where
--  ask = lift ask
--  {-# INLINE ask #-}
--  local f = lift . local f
--  {-# INLINE local #-}

instance (MonadState s m) => MonadState s (SkeletonT t m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance (MonadPlus m) => Alternative (SkeletonT t m) where
  empty = mzero
  SkeletonT ma <|> SkeletonT mb = SkeletonT $ ma `mplus` mb

instance (MonadPlus m) => MonadPlus (SkeletonT t m) where
  mzero = SkeletonT mzero
  {-# INLINE mzero #-}
  mplus (SkeletonT ma) (SkeletonT mb) = SkeletonT $ mplus ma mb

instance (MonadThrow m) => MonadThrow (SkeletonT t m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}
