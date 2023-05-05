{-# LANGUAGE NoImplicitPrelude #-}
module Data.Default (module Data.Default) where

import Custom.Prelude

import Data.Kind (Type)

import Control.ELens
import Control.PolyMonad
import Control.PolyMonad.ParserPrinter
import Control.PolyMonad.ProducerConsumer

import Data.Maybe (fromMaybe)

class WithDefault m where
  type Data m :: Type
  type Defaulted m :: Type -> Type
  withDefault :: Data m -> m a -> Defaulted m a

instance WithDefault (ConsumerT r m) where
  type Data (ConsumerT r m) = r
  type Defaulted (ConsumerT r m) = ConsumerT' r m
  withDefault r = ConsumerT' . mapC (fromMaybe r)

instance WithDefault m => WithDefault (PrinterT m) where
  type Data (PrinterT m) = Data m
  type Defaulted (PrinterT m) = PrinterT (Defaulted m)
  withDefault r = tmap (withDefault @m r)

instance WithDefault bwd => WithDefault (Lens fwd bwd a) where
  type Data (Lens fwd bwd a) = Data bwd
  type Defaulted (Lens fwd bwd a) = Lens fwd (Defaulted bwd) a
  withDefault r f = f{ from = withDefault r . from f }
