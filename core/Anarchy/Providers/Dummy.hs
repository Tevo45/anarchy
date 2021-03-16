module Anarchy.Providers.Dummy
  ( dummyProvider
  ) where

import Anarchy.Providers

import Control.Monad.Trans.Maybe

dummyProvider :: Champion -> Maybe Route -> MaybeT IO (Maybe Route, Rune)
dummyProvider _ r = return (r, Rune { primaryStyle = 8100
                                    , primaryPerks = (9923, 8139, 8138, 8135)
                                    , secondaryStyle = 8000
                                    , secondaryPerks = (8009, 9103)
                                    , fragments = (5005, 5008, 5001)
                                    })
