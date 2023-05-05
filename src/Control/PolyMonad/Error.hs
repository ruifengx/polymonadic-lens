{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad.Error
  ( module Control.PolyMonad.Error
  ) where

import Custom.Prelude

import Control.Monad.Identity (Identity (..), IdentityT (..))
import Control.Monad.Reader (ReaderT (..))
import Control.PolyMonad as Poly

newtype ErrorT e m a
  = ErrorT{ runErrorT :: m (Either e a) }
  deriving stock (Functor)

type Error e = ErrorT e Identity

runError :: Error e a -> Either e a
runError = runIdentity . runErrorT

instance FunctorLift (ErrorT e) where
  tlift = ErrorT . fmap Right
  tmap t = ErrorT . t . runErrorT

type BindError e m = (PolyMonad (Error e) m m, PolyMonad m (Error e) m)
class (Functor m, BindError e m) => ThrowError e m | m -> e where
  throwError :: e -> m a
  liftError :: ErrorT e m a -> m a

instance Pointed m => ThrowError e (ErrorT e m) where
  throwError = ErrorT . Poly.return . Left
  liftError m = ErrorT (join <$> runErrorT (runErrorT m))

-- ErrorT is a monad

instance (Pointed n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ErrorT e n) (ErrorT e p) where (<*>) = Poly.ap
instance (Pointed n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ErrorT e n) (ErrorT e p) where
  m >>= k = ErrorT $ runErrorT m Poly.>>= \case
    Left err -> Poly.return (Left err)
    Right x  -> runErrorT (k x)

instance Pointed m => Pointed (ErrorT e m) where
  return = ErrorT . Poly.return . Right

-- IdentityT implements ThrowError

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (IdentityT n) (IdentityT p) where (<*>) = Poly.ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (IdentityT n) (IdentityT p) where
  m >>= k = IdentityT Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runIdentityT (k a)

instance (Pointed n, ThrowError e p, PolyMonad m n p) => PolyApplicative (IdentityT m) (ErrorT e n) (IdentityT p) where (<*>) = Poly.ap
instance (Pointed n, ThrowError e p, PolyMonad m n p) => PolyMonad (IdentityT m) (ErrorT e n) (IdentityT p) where
  ~(IdentityT m) >>= k = IdentityT $ liftError $ ErrorT @e (fmap Right m) Poly.>>= k

instance ThrowError e m => ThrowError e (IdentityT m) where
  throwError = IdentityT . throwError
  liftError = IdentityT . liftError . ErrorT . runIdentityT . runErrorT

-- ReaderT implements ThrowError

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ReaderT r n) (ReaderT r p) where (<*>) = Poly.ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ReaderT r n) (ReaderT r p) where
  m >>= k = ReaderT \r -> Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runReaderT (k a) r

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (ReaderT r m) (ErrorT e n) (ReaderT r p) where (<*>) = Poly.ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (ReaderT r m) (ErrorT e n) (ReaderT r p) where
  m >>= k = liftError $ ErrorT $ ReaderT $ runReaderT m Poly.>=> runErrorT . k

instance ThrowError e m => ThrowError e (ReaderT r m) where
  throwError = ReaderT . const . throwError
  liftError = ReaderT . ((liftError . ErrorT) .) . runReaderT . runErrorT
