{-# LANGUAGE OverloadedStrings #-}
module Anarchy.Providers.OPGG
  (opggProvider) where

import Anarchy.Providers
import Anarchy.DataDragon

import Control.Monad.Trans.Maybe

import qualified Data.Text as T
import Data.List.Split
import Data.Maybe
import Data.Sort

import Text.HTML.Scalpel

data OPGGRuneStats = Stats { statsPickRatio :: Double
                           , statsPickCount :: Integer
                           , statsWinRate :: Double
                           } deriving Show

idFromPicUrl :: (Num a, Read a) => String -> a
idFromPicUrl = read . takeWhile (/= '.') . last . splitOn "/"

-- FIXME duplicated code

perkPage :: (Num a, Read a) => Scraper String [a] 
perkPage = do
    mainUrl <- attr "src" perk
    keystoneUrls <- chroots keystones $ attr "src" anySelector
    return . map idFromPicUrl $ mainUrl:keystoneUrls
  where
    perk =
      "div" @: [hasClass "perk-page__item--mark"] //
      "img"
    keystones =
      "div" @: [hasClass "perk-page__item--active"] //
      "img"

perkPages :: (Num a, Read a) => Scraper String [[a]]
perkPages = chroots ("div" @: [hasClass "perk-page"]) perkPage

fragmentPage :: (Num a, Read a) => Scraper String [a]  
fragmentPage = do
    fs <- chroots page $ attr "src" anySelector
    return . map idFromPicUrl $ fs
  where
    page =
      "div" @: [hasClass "fragment-page"] //
      "img" @: [hasClass "active"]

runePage :: Scraper String Rune
runePage = do
    ((pa:p1:p2:p3:p4:_):(sa:s1:s2:_):_) <- perkPages
    (f1:f2:f3:_) <- fragmentPage
    return $ Rune pa (p1, p2, p3, p4) sa (s1, s2) (f1, f2, f3)

runeStats :: Scraper String OPGGRuneStats
runeStats = inSerial $ do
    pickRatio <- seekNext $ text "strong"
    pickCount <- seekNext $ text "span"
    winRate   <- seekNext $ text "strong"
    return $ Stats (readWithout '%' pickRatio)
                   (readWithout ',' pickCount)
                   (readWithout '%' winRate)
  where
    readWithout c = read . filter (/= c)

runesFromPage :: Scraper String (Rune, OPGGRuneStats)
runesFromPage = do
    stats <- chroot ("td" @: [hasClass "champion-overview__stats"]) runeStats
    rune  <- chroot ("div" @: [hasClass "perk-page-wrap"]) runePage
    return (rune, stats)

runesetsFromPage :: Scraper String [(Rune, OPGGRuneStats)]
runesetsFromPage = chroots pageTable runesFromPage
  where
    pageTable =
      "table" @: [hasClass "champion-overview__table--rune"] //
      "tbody" @: [hasClass "tabItem"] //
      "tr"

-- FIXME if there's no data for the champ on the specified route, op.gg will
-- redirect us to the "default" route for the champ
opggProvider :: Champion -> Maybe Route -> MaybeT IO (Maybe Route, Rune)
opggProvider champ route = do
    name  <- getChampNameByKey (DDCDN riotCdnUrl "en_US" "11.5.1") champ -- FIXME
    runes <- MaybeT $ scrapeURL (url $ T.unpack name) runesetsFromPage
    return $ (route, betterPickRate runes)
  where
    url n = "https://na.op.gg/champion/" <> n <> "/statistics/" <> routeName route
    routeName Nothing = ""
    routeName (Just r) = case r of
      Top     -> "top"
      Middle  -> "mid"
      Bottom  -> "bot"
      Support -> "support"
      Jungle  -> "jungle"

    -- TODO make this configurable
    betterPickRate = fst . head . sortBy pickRate
      where
        pickRate (_, Stats p _ _) (_, Stats q _ _) = compare p q
