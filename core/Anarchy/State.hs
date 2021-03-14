module Anarchy.State
  ( AnarchyConfig(..)
  ) where

data AnarchyConfig = AnarchyConfig { enabledRuneProviders :: [String]
                                   } deriving Show
