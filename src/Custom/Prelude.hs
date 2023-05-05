module Custom.Prelude
  ( module Prelude
  , module Control.Category
  , module Control.Monad
  ) where

import Prelude hiding (Applicative (..), Monad (..), id, (.))

import Control.Category (Category (id, (.)))
import Control.Monad (join)
