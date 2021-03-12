module Anarchy.Providers
  ( Route(..)
  , Rune(..)
  , Champion
  , Provider
  ) where

import Control.Monad.Trans.Maybe

data Route = Top | Middle | Bottom | Support | Jungle
             deriving (Show, Eq)

data Rune = Rune { primaryStyle :: Int
                 , primaryPerks :: (Int, Int, Int, Int)
                 , secondaryStyle :: Int
                 , secondaryPerks :: (Int, Int)
                 , fragments :: (Int, Int, Int)
                 } deriving (Show, Eq)

type Champion = Int
type Provider = Champion -> Maybe Route -> MaybeT IO (Maybe Route, Rune)
