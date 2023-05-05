{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax    #-}
module Control.PolyMonad.Transducer
  ( TransducerT(..)
  , HList(.., (:++))
  , (>=>)
  ) where

import Custom.Prelude

import Control.Monad.Identity (Identity (..))
import Control.PolyMonad (Pointed (return), PolyMonad ((>>=)))
import Data.Kind (Type)

data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x : xs)

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : xs ++ ys

concatHList :: HList xs -> HList ys -> HList (xs ++ ys)
concatHList HNil ys         = ys
concatHList (HCons x xs) ys = HCons x (concatHList xs ys)

-- note: this is necessary, because types are erased at runtime
-- this type class is the place to remember where to break the `HList`
class HListSplit (xs :: [Type]) (ys :: [Type]) where
  splitHList :: HList (xs ++ ys) -> (HList xs, HList ys)

instance HListSplit '[] ys where
  splitHList ys = (HNil, ys)

instance HListSplit xs ys => HListSplit (x : xs) ys where
  splitHList (HCons x xys) = let (xs, ys) = splitHList xys in (HCons x xs, ys)

pattern (:++) :: HListSplit xs ys => HList xs -> HList ys -> HList (xs ++ ys)
pattern (:++) xs ys <- (splitHList -> (xs, ys))
  where (:++) = concatHList

{-# COMPLETE (:++) #-}

newtype TransducerT xs ys m a
  = TransducerT{ runTransducerT :: HList xs -> m (a, HList ys) }
  deriving stock (Functor)

-- note: this type is ambiguous, and cannot be an instance for the `PolyMonad` class
(>=>) :: forall xs ysP ysT ysC zs m n p a b c.
         ( HListSplit ysP ysT
         , HListSplit ysC ysT
         , HListSplit ysC xs
         , HListSplit ysP zs
         , PolyMonad m n p
         )
      => (a -> TransducerT xs (ysP ++ ysT) m b)
      -> (b -> TransducerT (ysC ++ ysT) zs n c)
      -> (a -> TransducerT (ysC ++ xs) (ysP ++ zs) p c)
(f >=> g) a = TransducerT \((ysC :: HList ysC) :++ xs) -> do
  ~(b, (ysP :: HList ysP) :++ (ysT :: HList ysT)) <- runTransducerT (f a) xs
  (c, zs) <- runTransducerT (g b) (ysC :++ ysT)
  return @Identity (c, ysP :++ zs)
