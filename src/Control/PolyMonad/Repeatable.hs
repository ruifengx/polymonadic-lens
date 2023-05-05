{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad.Repeatable
  ( module Control.PolyMonad.Repeatable
  ) where

import Custom.Prelude

import Control.PolyMonad
import Control.PolyMonad.Error
import Control.PolyMonad.ParserPrinter
import Control.PolyMonad.ProducerConsumer

import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (ReaderT (..))
import Data.Kind (Type)
import Data.List (uncons)

import qualified Control.Monad as M ((>>=))

class (Pointed (Repeated m), PolyApplicative m (Repeated m) (StepFwd m), PolyApplicative (Repeated m) m (StepBwd m)) => Repeatable m where
  type Repeated m :: Type -> Type
  type StepFwd m :: Type -> Type
  type StepBwd m :: Type -> Type
  morphStepFwd :: StepFwd m a -> Repeated m a
  morphStepBwd :: StepBwd m a -> Repeated m a

type RepeatableM m = (Repeatable m, PolyMonad m (Repeated m) (StepFwd m), PolyMonad (Repeated m) m (StepBwd m))

instance Repeatable Identity where
  type Repeated Identity = Identity
  type StepFwd Identity = Identity
  type StepBwd Identity = Identity
  morphStepFwd = id
  morphStepBwd = id

instance RepeatableM m => Repeatable (ProducerT w m) where
  type Repeated (ProducerT w m) = ProducerT [w] (Repeated m)
  type StepFwd (ProducerT w m) = ProducerT (w, [w]) (StepFwd m)
  type StepBwd (ProducerT w m) = ProducerT ([w], w) (StepBwd m)
  morphStepFwd = mapP (uncurry (:)) . ProducerT . morphStepFwd @m . runProducerT
  morphStepBwd = mapP (uncurry snoc) . ProducerT . morphStepBwd @m . runProducerT
    where snoc xs x = xs ++ [x]

instance RepeatableM m => Repeatable (ConsumerT' r m) where
  type Repeated (ConsumerT' r m) = ConsumerT' [r] (Repeated m)
  type StepFwd (ConsumerT' r m) = ConsumerT' ([r], r) (StepFwd m)
  type StepBwd (ConsumerT' r m) = ConsumerT' (r, [r]) (StepBwd m)
  morphStepFwd m = ConsumerT'
    $ mapC (M.>>= unsnoc)
    $ ConsumerT (morphStepFwd @m . runConsumerT' m)
    where   unsnoc [] = Nothing
            unsnoc (x : xs)
              | Just (ys, y) <- unsnoc xs = Just (x : ys, y)
              | otherwise = Just ([], x)
  morphStepBwd m = ConsumerT'
    $ mapC (M.>>= uncons)
    $ ConsumerT (morphStepBwd @m . runConsumerT' m)

instance RepeatableM m => Repeatable (ReaderT r m) where
  type Repeated (ReaderT r m) = ReaderT r (Repeated m)
  type StepFwd (ReaderT r m) = ReaderT r (StepFwd m)
  type StepBwd (ReaderT r m) = ReaderT r (StepBwd m)
  morphStepFwd = ReaderT . (morphStepFwd @m .) . runReaderT
  morphStepBwd = ReaderT . (morphStepBwd @m .) . runReaderT

instance (Pointed m, RepeatableM m) => Repeatable (ErrorT e m) where
  type Repeated (ErrorT e m) = ErrorT e (Repeated m)
  type StepFwd (ErrorT e m) = ErrorT e (StepFwd m)
  type StepBwd (ErrorT e m) = ErrorT e (StepBwd m)
  morphStepFwd = ErrorT . morphStepFwd @m . runErrorT
  morphStepBwd = ErrorT . morphStepBwd @m . runErrorT

instance RepeatableM m => Repeatable (ParserT m) where
  type Repeated (ParserT m) = ParserT (Repeated m)
  type StepFwd (ParserT m) = ParserT (StepFwd m)
  type StepBwd (ParserT m) = ParserT (StepBwd m)
  morphStepFwd = ParserT . (morphStepFwd @m .) . runParserT
  morphStepBwd = ParserT . (morphStepBwd @m .) . runParserT

instance RepeatableM m => Repeatable (PrinterT m) where
  type Repeated (PrinterT m) = PrinterT (Repeated m)
  type StepFwd (PrinterT m) = PrinterT (StepFwd m)
  type StepBwd (PrinterT m) = PrinterT (StepBwd m)
  morphStepFwd = PrinterT . (morphStepFwd @m .) . runPrinterT
  morphStepBwd = PrinterT . (morphStepBwd @m .) . runPrinterT

instance Repeatable m => Repeatable (Backwards m) where
  type Repeated (Backwards m) = Backwards (Repeated m)
  type StepFwd (Backwards m) = Backwards (StepBwd m)
  type StepBwd (Backwards m) = Backwards (StepFwd m)
  morphStepFwd = Backwards . morphStepBwd @m . forwards
  morphStepBwd = Backwards . morphStepFwd @m . forwards
