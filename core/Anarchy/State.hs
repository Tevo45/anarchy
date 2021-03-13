module Anarchy.State where

data AnarchyConfig = AnarchyConfig { enabledRuneProviders :: [String]
                                   } deriving Show
