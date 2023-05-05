{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.PolyMonad.ParserCombinators
  ( module Control.PolyMonad.ParserCombinators
  ) where

import Custom.Prelude hiding (words)

import Control.Alternative3
import Control.ELens
import Control.PolyMonad
import Control.PolyMonad.Error
import Control.PolyMonad.ParserPrinter
import Control.PolyMonad.ProducerConsumer
import Control.PolyMonad.Repeatable (Repeatable (..))
import Data.Default (WithDefault (..))

import Control.Monad.Identity (Identity (..))
import Data.Char (isLetter)
import Data.Functor (void)
import Data.Tuple (swap)
import Text.Printf (printf)

pAnyChar :: (ThrowError String fwd, Pointed fwd, Pointed bwd)
  => Lens (ParserT fwd) (PrinterT bwd) () Char
pAnyChar = Lens{ to = const (satisfy (const True)), from = printChar }

pSatisfy :: (ThrowError String fwd, ThrowError String bwd, Pointed fwd, Pointed bwd)
  => (Char -> Bool) -> Lens (ParserT fwd) (PrinterT bwd) () Char
pSatisfy p = Lens{ to = const (satisfy p), from }
  where from c | p c = printChar c
               | otherwise = throwError "pSatisfy.from: not satisfied"

(<<*>>) :: (PolyApplicative f1 f2 fwd, PolyApplicative b2 b1 bwd)
  => Lens f1 b1 x1 y1
  -> Lens f2 b2 x2 y2
  -> Lens fwd bwd (x1, x2) (y1, y2)
f <<*>> g = Lens
  { to = \(x1, x2) -> (,) <$> to f x1 <*> to g x2
  , from = \(y1, y2) -> fmap swap $ (,) <$> from g y2 <*> from f y1
  }

nil :: (Pointed fwd, Pointed bwd, ThrowError String bwd)
  => Lens fwd bwd () [a]
nil = Lens{ to, from }
  where to _ = return []
        from = void . assert null "nil.from: not null"

bDupl :: Lens Identity Identity () ((), ())
bDupl = Lens{ to = Identity . const ((), ()), from = Identity . const () }

bCons :: Lens Identity (Error String) (a, [a]) [a]
bCons = Lens{ to = return . uncurry (:), from }
  where from []       = throwError "bCons.from: empty list"
        from (x : xs) = return (x, xs)

morphRepeated :: forall fwd bwd a b. (Repeatable fwd, Repeatable bwd)
  => Lens (StepFwd fwd) (StepBwd bwd) a b
  -> Lens (Repeated fwd) (Repeated bwd) a b
morphRepeated f = Lens{ to = morphStepFwd @fwd . to f, from = morphStepBwd @bwd . from f }

manyWith, someWith :: forall fwd bwd fwd' bwd' a.
  ( Repeatable fwd
  , Repeatable bwd
  , ThrowError String (Repeated bwd)
  , Alternative3 (Repeated fwd) (Repeated fwd) fwd'
  , Alternative3 (Repeated bwd) (Repeated bwd) bwd'
  ) => (forall x. Lens fwd' bwd' () x -> Lens (Repeated fwd) (Repeated bwd) () x)
  -> Lens fwd bwd () a -> Lens (Repeated fwd) (Repeated bwd) () [a]
manyWith t p = t (someWith t p <|> nil @(Repeated fwd) @(Repeated bwd))
someWith t p = bCons % morphRepeated @fwd @bwd (p <<*>> manyWith t p) % bDupl

many, some :: forall fwd bwd a.
  ( Repeatable fwd
  , Repeatable bwd
  , ThrowError String (Repeated bwd)
  , Alternative3 (Repeated fwd) (Repeated fwd) (Repeated fwd)
  , Alternative3 (Repeated bwd) (Repeated bwd) (Repeated bwd)
  ) => Lens fwd bwd () a -> Lens (Repeated fwd) (Repeated bwd) () [a]
many = manyWith id
some = someWith id

type BParser = BParserLike ()
type BParserLike = Lens (ParserT (Error String)) (PrinterT (Error String))

type BParserWith w = BParserLikeWith w ()
type BParserLikeWith w = Lens
  (ParserT (ProducerT w (Error String)))
  (PrinterT (ConsumerT w (Error String)))

type BParserWith' w = BParserLikeWith' w ()
type BParserLikeWith' w = Lens
  (ParserT (ProducerT w (Error String)))
  (PrinterT (ConsumerT' w (Error String)))

pMapA :: (u -> v) -> (v -> u) -> BParserWith u a -> BParserWith v a
pMapA f g p = Lens{ to = to', from = from' }
  where   to' = tmap (mapP f) . to p
          from' = tmap (mapC g) . from p

pMapA' :: (u -> v) -> (v -> u) -> BParserWith' u a -> BParserWith' v a
pMapA' f g p = Lens{ to = to', from = from' }
  where   to' = tmap (mapP f) . to p
          from' = tmap (mapC' g) . from p

dropA :: BParser a -> BParserWith a ()
dropA p = go % p
  where   go :: BParserLikeWith a a ()
          go = Lens{ to = to', from = from' }
          to' a = tlift (produce a)
          from' () = tlift consume

spaces :: BParser String
spaces = many (pSatisfy @(Error String) @(Error String) (not . isLetter))

sepBy :: b -> BParser a -> BParser b -> BParserWith' [b] [a]
sepBy d p sep = pMapA' (either id id) wrap
  $ sepBy1 d p sep <|> (nil :: BParserWith' [b] [a])
  where wrap bs = if null bs then Right bs else Left bs

(*>>) :: (PolyApplicative f1 f2 fwd, PolyApplicative b2 b1 bwd)
  => Lens f1 b1 () ()
  -> Lens f2 b2 x y
  -> Lens fwd bwd x y
f *>> g = Lens
  { to = \x -> to f () *> to g x
  , from = \y -> from g y <* from f ()
  }

(<<*) :: (PolyApplicative f1 f2 fwd, PolyApplicative b2 b1 bwd)
  => Lens f1 b1 x y
  -> Lens f2 b2 () ()
  -> Lens fwd bwd x y
f <<* g = Lens
  { to = \x -> to f x <* to g ()
  , from = \y -> from g () *> from f y
  }

sepBy1 :: b -> BParser a -> BParser b -> BParserWith' [b] [a]
sepBy1 d p sep = bCons % (p <<*>> q) % bDupl
  where q = manyWith wrap (withDefault d (dropA sep *>> p))
        wrap :: BParserWith' (Either [a] [a]) b -> BParserWith' [a] b
        wrap = pMapA' (either id id) (\xs -> if null xs then Right xs else Left xs)

eof :: BParser ()
eof = Lens{ to, from }
  where   to () = ParserT \case
            "" -> return ((), "")
            _  -> throwError "eof.to: end of input expected"
          from () = PrinterT \case
            "" -> return ((), "")
            _  -> throwError "eof.from: empty output expected"

parser :: (ThrowError String fwd, ThrowError String bwd)
  => Lens (ParserT fwd) (PrinterT bwd) () a -> Lens fwd bwd String a
parser p = Lens{ to = to', from = from' }
  where   to' = runParserT (to p ()) >=> assertEof
          from' = fmap snd . flip runPrinterT "" . from p
          assertEof (x, "") = return @(Error String) x
          assertEof (_, s)  = throwError (printf "parser.parse: EOF expected, got \"%s\" instead" s)

words :: ALens' [String] (Error String) (Error String) String [String]
words = parser (sepBy " " letters spaces)
  where letters = some (pSatisfy @(Error String) @(Error String) isLetter)

type ALensInv w fwd bwd a b = Lens (ConsumerT w fwd) (ProducerT w bwd) a b
type ALensInv' w fwd bwd a b = Lens (ConsumerT' w fwd) (ProducerT w bwd) a b

unwords :: ALensInv' [String] (Error String) (Error String) [String] String
unwords = inv words
