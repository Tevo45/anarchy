{-# LANGUAGE OverloadedStrings, DataKinds, RankNTypes, KindSignatures, DeriveGeneric #-}
module Anarchy.DataDragon
  ( DDCDN(..)
  , DDSource(..)
  , riotCdnUrl
  , latestDdVersion
  , getChampNameByKey
  ) where

import GHC.Generics

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Aeson
import Data.Maybe

import Network.HTTP.Req

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

class DDSource c where
  fetchJsonResource :: FromJSON a => c -> [T.Text] -> IO a

data DDCDN = DDCDN { cdnBaseUrl :: Url 'Https -- FIXME
                   , cdnLang :: T.Text
                   , cdnVersion :: T.Text
                   }

riotCdnUrl :: Url 'Https
riotCdnUrl = https "ddragon.leagueoflegends.com" /: "cdn"

latestDdVersion :: MaybeT IO T.Text
latestDdVersion = MaybeT . runReq defaultHttpConfig $ do
    versions <- req GET
      (https "ddragon.leagueoflegends.com" /: "api" /: "versions.json")
      NoReqBody jsonResponse mempty
    return $ head <$> responseBody versions

instance DDSource DDCDN where
  fetchJsonResource cdn path = runReq defaultHttpConfig $
        responseBody <$> req GET url NoReqBody jsonResponse mempty
    where
      base = (cdnBaseUrl cdn) /: (cdnVersion cdn) /: "data" /: (cdnLang cdn)
      url = (foldl (/:) base path)

data ChampTable = ChampTable { keys :: M.Map String T.Text}
                deriving (Generic, Show)

instance FromJSON ChampTable

-- TODO cache this
getChampNameByKey :: (Show a, Num a, DDSource c) => c -> a -> MaybeT IO T.Text
getChampNameByKey client key = do
    table <- MaybeT $ fetchJsonResource client ["championFull.json"]
    MaybeT . return $ show key `M.lookup` keys table
