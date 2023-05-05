{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QualifiedDo          #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Alternative3
  ( module Control.Alternative3
  ) where

import Custom.Prelude

import Control.ELens
import Control.PolyMonad as Poly
import Control.PolyMonad.Error
import Control.PolyMonad.ParserPrinter
import Control.PolyMonad.ProducerConsumer

import Control.Monad.Identity (Identity (..))
import Text.Printf (printf)

import qualified Control.Monad as M ((>>=))

class Alternative3 l r m | l r -> m where
  (<|>) :: l a -> r a -> m a

instance (Functor lm, Functor rm, Alternative3 lm rm m)
  => Alternative3 (ProducerT lw lm) (ProducerT rw rm) (ProducerT (Either lw rw) m) where
  l <|> r = ProducerT (runProducerT (mapP Left l) <|> runProducerT (mapP Right r))

instance Alternative3 lm rm m => Alternative3 (ConsumerT' lw lm) (ConsumerT' rw rm) (ConsumerT' (Either lw rw) m) where
  l <|> r = MkConsumerT' \w ->
    let lw = w M.>>= left; rw = w M.>>= right
    in runConsumerT' l lw <|> runConsumerT' r rw
    where left = either Just (const Nothing)
          right = either (const Nothing) Just

instance (Alternative3 fl fr fwd, Alternative3 bl br bwd)
  => Alternative3 (Lens fl bl a) (Lens fr br a) (Lens fwd bwd a) where
  f <|> g = Lens{ to = to', from = from' }
    where to' x = to f x <|> to g x
          from' y = from f y <|> from g y

instance PolyMonad m n p => Alternative3 (ErrorT String m) (ErrorT String n) (ErrorT String p) where
  l <|> r = ErrorT Poly.do
    xl <- runErrorT l
    xr <- runErrorT r
    id @_ @(Identity _) case (xl, xr) of
      (Right x, _)       -> return (Right x)
      (_, Right y)       -> return (Right y)
      (Left ex, Left ey) -> return (Left (printf msg ex ey))
    where msg = "ErrorT.(<|>): both branches failed:\n  left:  %s\n  right: %s"

instance Alternative3 lm rm m => Alternative3 (ParserT lm) (ParserT rm) (ParserT m) where
  p <|> q = ParserT \s -> runParserT p s <|> runParserT q s

instance Alternative3 lm rm m => Alternative3 (PrinterT lm) (PrinterT rm) (PrinterT m) where
  p <|> q = PrinterT \s -> runPrinterT p s <|> runPrinterT q s
