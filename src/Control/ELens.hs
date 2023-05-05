{-# LANGUAGE NoImplicitPrelude #-}
module Control.ELens (module Control.ELens) where

import Custom.Prelude

import Control.PolyMonad
import Control.PolyMonad.Error
import Control.PolyMonad.ProducerConsumer

data Lens fwd bwd a b = Lens{ to :: a -> fwd b, from  :: b -> bwd a }

(%) :: (PolyMonad f1 f2 f3, PolyMonad b2 b1 b3) => Lens f2 b2 b c -> Lens f1 b1 a b -> Lens f3 b3 a c
f % g = Lens{ to = to g >=> to f, from = from f >=> from g }

idLens :: (Pointed fwd, Pointed bwd) => Lens fwd bwd a a
idLens = Lens{ to = return, from = return }

inv :: Lens fwd bwd a b -> Lens bwd fwd b a
inv f = Lens{ from = to f, to = from f }

type ALens c fwd bwd a b = Lens (ProducerT c fwd) (ConsumerT c bwd) a b

mapA :: Functor fwd
  => (u -> v) -> (v -> u)
  -> ALens u fwd bwd a b
  -> ALens v fwd bwd a b
mapA f g l = Lens { to = mapP f . to l, from = mapC g . from l }

type ALens' w fwd bwd a b = Lens (ProducerT w fwd) (ConsumerT' w bwd) a b

bAssert :: forall fwd bwd a. (ThrowError String fwd, ThrowError String bwd, Pointed fwd, Pointed bwd)
  => (a -> Bool) -> Lens fwd bwd a a
bAssert p = Lens{ to = go, from = go }
  where   go :: (ThrowError String m, Pointed m) => a -> m a
          go = assert p "bAssert: predicate not satisfied"

assert :: (ThrowError e m, Pointed m) => (a -> Bool) -> e -> a -> m a
assert p msg x = if p x then return x else throwError msg
