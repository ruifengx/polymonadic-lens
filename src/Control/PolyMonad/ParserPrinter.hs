{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad.ParserPrinter
  ( module Control.PolyMonad.ParserPrinter
  ) where

import Custom.Prelude

import Control.PolyMonad as Poly
import Control.PolyMonad.Error

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (StateT (..))
import Data.Bifunctor (Bifunctor (..))

newtype ParserT m a = ParserT{ runParserT :: String -> m (a, String) }

type Parser = ParserT Identity

satisfy :: (Pointed m, ThrowError String m) => (Char -> Bool) -> ParserT m Char
satisfy p = ParserT \case
  [] -> throwError "satisfy: input exhausted"
  (x : xs)  | p x -> return (x, xs)
            | otherwise -> throwError "satisfy: predicate not satisfied"

instance Functor m => Functor (ParserT m) where
  fmap f (ParserT r) = ParserT (fmap (first f) . r)

instance Pointed m => Pointed (ParserT m) where
  return a = ParserT \s -> return (a, s)

instance FunctorLift ParserT where
  tlift m = ParserT \s -> fmap (, s) m
  tmap t = ParserT . (t .) . runParserT

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (ParserT n) (ParserT p) where (<*>) = ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (ParserT n) (ParserT p) where
  m >>= k = ParserT \s -> Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runParserT (k a) s

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (ParserT m) (ErrorT e n) (ParserT p) where (<*>) = ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (ParserT m) (ErrorT e n) (ParserT p) where
  m >>= k = liftError $ ErrorT $ ParserT \s -> Poly.do
    (a, s') <- runParserT m s
    (, s') <$> runErrorT (k a)

instance ThrowError e m => ThrowError e (ParserT m) where
  throwError = ParserT . const . throwError
  liftError = ParserT . ((liftError . ErrorT . fmap \(x, w) -> fmap (, w) x) .) . runParserT . runErrorT

instance PolyMonad m n p => PolyApplicative (ParserT m) (ParserT n) (ParserT p) where (<*>) = ap
instance PolyMonad m n p => PolyMonad (ParserT m) (ParserT n) (ParserT p) where
  p >>= f = ParserT \s -> Poly.do
    (a, s') <- runParserT p s
    (b, s'') <- runParserT (f a) s'
    return @Identity (b, s'')

newtype PrinterT m a = PrinterT{ runPrinterT :: String -> m (a, String) }

type Printer = PrinterT Identity

printChar :: Pointed m => Char -> PrinterT m ()
printChar c = PrinterT \s -> return ((), c : s)

instance Functor m => Functor (PrinterT m) where
  fmap f (PrinterT r) = PrinterT (fmap (first f) . r)

instance Pointed m => Pointed (PrinterT m) where
  return a = PrinterT \s -> return (a, s)

instance FunctorLift PrinterT where
  tlift m = PrinterT \s -> fmap (, s) m
  tmap t = PrinterT . (t .) . runPrinterT

instance (ThrowError e n, PolyMonad m n p) => PolyApplicative (ErrorT e m) (PrinterT n) (PrinterT p) where (<*>) = ap
instance (ThrowError e n, PolyMonad m n p) => PolyMonad (ErrorT e m) (PrinterT n) (PrinterT p) where
  m >>= k = PrinterT \s -> Poly.do
    x <- runErrorT m
    case x of
      Left e  -> throwError e
      Right a -> runPrinterT (k a) s

instance (ThrowError e p, PolyMonad m n p) => PolyApplicative (PrinterT m) (ErrorT e n) (PrinterT p) where (<*>) = ap
instance (ThrowError e p, PolyMonad m n p) => PolyMonad (PrinterT m) (ErrorT e n) (PrinterT p) where
  m >>= k = liftError $ ErrorT $ PrinterT \s -> Poly.do
    (a, s') <- runPrinterT m s
    (, s') <$> runErrorT (k a)

instance ThrowError e m => ThrowError e (PrinterT m) where
  throwError = PrinterT . const . throwError
  liftError = PrinterT . ((liftError . ErrorT . fmap \(x, w) -> fmap (, w) x) .) . runPrinterT . runErrorT

instance PolyMonad m n p => PolyApplicative (PrinterT m) (PrinterT n) (PrinterT p) where (<*>) = ap
instance PolyMonad m n p => PolyMonad (PrinterT m) (PrinterT n) (PrinterT p) where
  p >>= f = PrinterT \s -> Poly.do
    (a, s') <- runPrinterT p s
    (b, s'') <- runPrinterT (f a) s'
    return @Identity (b, s'')

instance PolyMonad m n p => PolyApplicative (ParserT m) (PrinterT n) (StateT String p) where (<*>) = ap
instance PolyMonad m n p => PolyMonad (ParserT m) (PrinterT n) (StateT String p) where
  p >>= f = StateT \s -> Poly.do
    (a, s') <- runParserT p s
    (b, s'') <- runPrinterT (f a) s'
    return @Identity (b, s'')
