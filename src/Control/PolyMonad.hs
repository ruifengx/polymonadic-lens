{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad (module Control.PolyMonad) where

import Control.Applicative.Backwards (Backwards (..))
import Control.Category (Category (..))
import Control.Monad.Identity (Identity (..), IdentityT (..))
import Control.Monad.Reader (ReaderT (..))

import Prelude hiding (Applicative (..), Monad (..), id, (.))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

-- Lifting functors (functor composition)

class (forall f. Functor f => Functor (t f)) => FunctorLift t where
  tlift :: Functor f => f a -> t f a
  tmap :: (forall x. f x -> g x) -> t f a -> t g a

-- Polymonad version of Applicative

infixl 4 <*>, <*, *>

class (Functor m, Functor n, Functor p) => PolyApplicative m n p | m n -> p where
  (<*>) :: m (a -> b) -> n a -> p b

ap :: PolyMonad m n p => m (t -> b) -> n t -> p b
ap mf mx = Control.PolyMonad.do f <- mf; fmap f mx

(*>) :: PolyApplicative m n p => m a -> n b -> p b
ma *> nb = id <$ ma <*> nb

(<*) :: PolyApplicative m n p => m a -> n b -> p a
ma <* nb = const <$> ma <*> nb

-- Polymonad itself

infixl 1 >>=, >>

class PolyApplicative m n p => PolyMonad m n p | m n -> p where
  (>>=) :: m a -> (a -> n b) -> p b
  (>>) :: m a -> n b -> p b

  m >> n = m >>= const n

infixr 1 >=>

(>=>) :: PolyMonad m n p => (a -> m b) -> (b -> n c) -> a -> p c
f >=> g = \x -> f x >>= g

-- instances: compose Identity onto any functor

instance Functor f => PolyApplicative f Identity f where
  f <*> Identity x = fmap ($ x) f

instance Functor f => PolyApplicative Identity f f where
  Identity f <*> x = f <$> x

instance Functor f => PolyMonad f Identity f where
  m >>= f = fmap (runIdentity . f) m

instance Functor f => PolyMonad Identity f f where
  Identity m >>= f = f m

-- pointed functors: 'return'

class Functor f => Pointed f where
  return :: a -> f a

instance Pointed Identity where
  return = Identity

-- instances: ReaderT

instance PolyMonad m n p => PolyApplicative (ReaderT r m) (ReaderT r n) (ReaderT r p) where (<*>) = ap
instance PolyMonad m n p => PolyMonad (ReaderT r m) (ReaderT r n) (ReaderT r p) where
  m >>= k = ReaderT \r -> Control.PolyMonad.do
    a <- runReaderT m r
    b <- runReaderT (k a) r
    return @Identity b

instance Pointed m => Pointed (ReaderT r m) where
  return a = ReaderT (return . const a)

asks :: Pointed m => (r -> a) -> ReaderT r m a
asks f = ReaderT (return . f)

ask :: Pointed m => ReaderT r m r
ask = asks id

-- instances IdentityT

instance PolyMonad m n p => PolyApplicative (IdentityT m) (IdentityT n) (IdentityT p) where (<*>) = ap
instance PolyMonad m n p => PolyMonad (IdentityT m) (IdentityT n) (IdentityT p) where
  m >>= k = IdentityT Control.PolyMonad.do
    a <- runIdentityT m
    b <- runIdentityT (k a)
    return @Identity b

instance Pointed m => Pointed (IdentityT m) where
  return = IdentityT . return

lift :: m a -> IdentityT m a
lift = IdentityT

unlift :: IdentityT m a -> m a
unlift = runIdentityT

-- instances Backwards

instance Pointed m => Pointed (Backwards m) where
  return = Backwards . return

instance PolyApplicative m n p => PolyApplicative (Backwards n) (Backwards m) (Backwards p) where
  Backwards nf <*> Backwards mx = Backwards $ flip ($) <$> mx <*> nf
