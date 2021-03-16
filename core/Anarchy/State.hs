-- Hypocrisy!
module Anarchy.State
  ( AnarchyConfig(..)
  , UIMessage(..)
  ) where

import Anarchy.Providers

data AnarchyConfig = AnarchyConfig { enabledRuneProviders :: [String]
                                   } deriving Show

data UIMessage = PickedRune Champion (Maybe Route) Rune
               | LCUConnecting
               | LCUConnectRetry Int
               | LCUConnected
               | OutOfChampSelect
               | ARError String -- FIXME improve error handling
               deriving Show
