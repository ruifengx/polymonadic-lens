{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad.ProducerConsumer
  ( module Control.PolyMonad.ProducerConsumer
  ) where

import Custom.Prelude

import Control.PolyMonad as Poly
import Control.PolyMonad.Error (ErrorT (..), ThrowError (..))

import Control.Arrow ((&&&))
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor (Bifunctor (..))

newtype ProducerT w m a
  = ProducerT{ runProducerT :: m (a, w) }
  deriving stock (Functor)

type Producer w = ProducerT w Identity

runProducer :: Producer w a -> (a, w)
runProducer = runIdentity . runProducerT

produce :: Pointed m => w -> ProducerT w m ()
produce = ProducerT . Poly.return . ((), )

newtype ConsumerT r m a
  = ConsumerT{ runConsumerT :: r -> m a }
  deriving stock (Functor)

type Consumer w = ConsumerT w Identity

runConsumer :: Consumer r a -> r -> a
runConsumer m = runIdentity . runConsumerT m

consume :: Pointed m => ConsumerT w m w
consume = ConsumerT Poly.return

instance PolyMonad m n p => PolyApplicative (ProducerT w1 m) (ProducerT w2 n) (ProducerT (w1, w2) p) where (<*>) = Poly.ap
instance PolyMonad m n p => PolyMonad (ProducerT w1 m) (ProducerT w2 n) (ProducerT (w1, w2) p) where
  m >>= k = ProducerT Poly.do
    (a, w1) <- runProducerT m
    (b, w2) <- runProducerT (k a)
    Poly.return @Identity (b, (w1, w2))

instance PolyMonad m n p => PolyApplicative (ConsumerT r2 m) (ConsumerT r1 n) (ConsumerT (r1, r2) p) where (<*>) = Poly.ap
instance PolyMonad m n p => PolyMonad (ConsumerT r2 m) (ConsumerT r1 n) (ConsumerT (r1, r2) p) where
  m >>= k = ConsumerT \(r1, r2) -> Poly.do
    a <- runConsumerT m r2
    runConsumerT (k a) r1

instance PolyMonad m n p => PolyApplicative (ProducerT s m) (ConsumerT s n) p where (<*>) = Poly.ap
instance PolyMonad m n p => PolyMonad (ProducerT s m) (ConsumerT s n) p where
  m >>= k = Poly.do
    (a, s) <- runProducerT m
    runConsumerT (k a) s

mapP :: Functor m => (u -> v) -> ProducerT u m a -> ProducerT v m a
mapP f = ProducerT . fmap (second f) . runProducerT

mapC :: (v -> u) -> ConsumerT u m a -> ConsumerT v m a
mapC g m = ConsumerT (runConsumerT m . g)

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ProducerT w n) (ProducerT w p) where (<*>) = Poly.ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ProducerT w n) (ProducerT w p) where
  m >>= k = ProducerT Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runProducerT (k a)

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (ProducerT w m) (ErrorT e n) (ProducerT w p) where (<*>) = Poly.ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (ProducerT w m) (ErrorT e n) (ProducerT w p) where
  m >>= k = liftError $ ErrorT $ ProducerT Poly.do
    (a, w) <- runProducerT m
    (, w) <$> runErrorT (k a)

instance ThrowError e m => ThrowError e (ProducerT w m) where
  throwError = ProducerT . throwError
  liftError = ProducerT . liftError . ErrorT . fmap (\(x, w) -> fmap (, w) x) . runProducerT . runErrorT

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ConsumerT r n) (ConsumerT r p) where (<*>) = Poly.ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ConsumerT r n) (ConsumerT r p) where
  m >>= k = ConsumerT \r -> Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runConsumerT (k a) r

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (ConsumerT r m) (ErrorT e n) (ConsumerT r p) where (<*>) = Poly.ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (ConsumerT r m) (ErrorT e n) (ConsumerT r p) where
  m >>= k = liftError $ ErrorT $ ConsumerT $ runConsumerT m Poly.>=> runErrorT . k

instance ThrowError e m => ThrowError e (ConsumerT r m) where
  throwError = ConsumerT . const . throwError
  liftError = ConsumerT . ((liftError . ErrorT) .) . runConsumerT . runErrorT

newtype ConsumerT' r m a = ConsumerT'{ unwrapConsumerT' :: ConsumerT (Maybe r) m a }
  deriving newtype (Functor)

consume' :: Pointed m => ConsumerT' w m (Maybe w)
consume' = MkConsumerT' Poly.return

pattern MkConsumerT' :: (Maybe r -> m a) -> ConsumerT' r m a
pattern MkConsumerT' k = ConsumerT' (ConsumerT k)

mapC' :: (v -> u) -> ConsumerT' u m a -> ConsumerT' v m a
mapC' g = ConsumerT' . mapC (fmap g) . unwrapConsumerT'

runConsumerT' :: ConsumerT' r m a -> Maybe r -> m a
runConsumerT' = runConsumerT . unwrapConsumerT'

instance PolyMonad m n p => PolyApplicative (ConsumerT' r2 m) (ConsumerT' r1 n) (ConsumerT' (r1, r2) p) where (<*>) = Poly.ap
instance PolyMonad m n p => PolyMonad (ConsumerT' r2 m) (ConsumerT' r1 n) (ConsumerT' (r1, r2) p) where
  m >>= f = ConsumerT' $ mapC (fmap fst &&& fmap snd) $ unwrapConsumerT' m Poly.>>= unwrapConsumerT' . f

instance PolyMonad m n p => PolyApplicative (ProducerT w m) (ConsumerT' w n) p where (<*>) = Poly.ap
instance PolyMonad m n p => PolyMonad (ProducerT w m) (ConsumerT' w n) p where
  m >>= k = mapP Just m Poly.>>= unwrapConsumerT' . k

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ConsumerT' r n) (ConsumerT' r p) where (<*>) = Poly.ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ConsumerT' r n) (ConsumerT' r p) where
  m >>= k = ConsumerT' (m Poly.>>= unwrapConsumerT' . k)

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (ConsumerT' r m) (ErrorT e n) (ConsumerT' r p) where (<*>) = Poly.ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (ConsumerT' r m) (ErrorT e n) (ConsumerT' r p) where
  m >>= k = ConsumerT' (unwrapConsumerT' m Poly.>>= k)

instance ThrowError e m => ThrowError e (ConsumerT' w m) where
  throwError = ConsumerT' . throwError
  liftError = ConsumerT' . liftError . tmap unwrapConsumerT'

instance (Monoid w, Pointed m) => Pointed (ProducerT w m) where
  return = ProducerT . Poly.return . (, mempty)

instance Pointed m => Pointed (ConsumerT r m) where
  return = ConsumerT . const . Poly.return

instance Pointed m => Pointed (ConsumerT' r m) where
  return = MkConsumerT' . const . Poly.return
