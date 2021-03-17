{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}
{-|
Module      : Anarchy
Description : Ties together backend and frontend communication logic
Copyright   : (c) Estevan Castilho, 2021
Maintainer  : estevan.cps@gmail.com
Portability : Windows, POSIX
-}
module Anarchy where

import Anarchy.DataDragon
import Anarchy.State
import Anarchy.LCU
import Anarchy.Providers
import Anarchy.Providers.Dummy
import Anarchy.Providers.OPGG

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.IORef

import Text.Regex.PCRE

import Network.WebSockets.Connection
import Network.HTTP.Req

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Morph
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar

-- | Metadata about a provider, used in the 'providers' table
data ProviderMeta = ProviderMeta { interfaceName :: String
                                 -- ^ Name to be displayed in the UI
                                 , description :: String
                                 -- ^ Short description
                                 , providerFunction :: Provider
                                 }

instance Show ProviderMeta where
  show ProviderMeta { interfaceName = name
                    , description = desc
                    } = name ++ ": " ++ desc
  
-- | Table containing all known (enabled) providers together with some metadata
providers :: [(String, ProviderMeta)]
providers = [("dummy", ProviderMeta { interfaceName = "Dummy"
                                    , description = "Useless provider"
                                    , providerFunction = dummyProvider
                                    })
            ,("op.gg", ProviderMeta { interfaceName = "OP.GG"
                                    , description = "Scrape runes from OP.GG"
                                    , providerFunction = opggProvider
                                    })
            ]

-- | Represents an LCU WAMP event payload
data EventPayload = Payload { eventData :: Object
                            , eventType :: String
                            , eventUri  :: String
                            } deriving Show

-- | Auxiliary type representing the current state of the autorune system
data AutoRuneState = Unhandled | Handled Champion
                   deriving Show

-- TODO we probably don't need all those fields
-- | Rune page as serialized by the LCU;
-- note that not all LCU-available fields are represented here
data RunePage = RunePage { pageIsCurrent :: Bool
                         , pageId :: Integer
                         , pageIsActive :: Bool
                         , pageIsDeletable :: Bool
                         , pageIsEditable :: Bool
                         , pageIsValid :: Bool
                         , pageLastModified :: Integer
                         , pageName :: String
                         , pageOrder :: Integer
                         , pageRune :: Rune
                         } deriving Show

-- | 'RunePage' instance with "default values" filled in
defaultPage :: RunePage
defaultPage = RunePage False 0 True True True True 0 "" 0 r
  where
    r = Rune 0 (0, 0, 0, 0) 0 (0, 0) (0, 0, 0)

-- | Parses part of an LCU rune page object into a 'Rune'
instance FromJSON Rune where
  parseJSON (Object o) = Rune <$> o .: "primaryStyleId"
                              <*> (t4 <$> perks)
                              <*> o .: "subStyleId"
                              <*> (t2 <$> drop 4 <$> perks)
                              <*> (t3 <$> drop 6 <$> perks)
    where
      t4 (a:b:c:d:_) = (a,b,c,d)
      t3 (a:b:c:_) = (a,b,c)
      t2 (a:b:_) = (a,b)
      perks = o .: "selectedPerkIds"

-- | Parses an LCU rune page into a 'RunePage', including the rune itself
instance FromJSON RunePage where
  parseJSON (Object o) = RunePage <$> o .: "current"
                                  <*> o .: "id"
                                  <*> o .: "isActive"
                                  <*> o .: "isDeletable"
                                  <*> o .: "isEditable"
                                  <*> o .: "isValid"
                                  <*> o .: "lastModified"
                                  <*> o .: "name"
                                  <*> o .: "order"
                                  <*> parseJSON (Object o)

-- | Serializes a 'RunePage', including the rune, into an LCU-compatible object
instance ToJSON RunePage where
  toJSON (RunePage cur pid active del edit valid lastMod name order rune) =
    object [ "current" .= cur
           , "id" .= pid
           , "isActive" .= active
           , "isDeletable" .= del
           , "isEditable" .= edit
           , "isValid" .= valid
           , "lastModified" .= lastMod
           , "name" .= name
           , "order" .= order
           , "primaryStyleId" .= primary
           , "subStyleId" .= secondary
           , "selectedPerkIds" .= (p1, p2, p3, p4, s1, s2, f1, f2, f3)
           ]
    where
      (Rune primary (p1, p2, p3, p4) secondary (s1, s2) (f1, f2, f3)) = rune

-- | Parses an LCU WAMP event payload object into an 'EventPayload'
instance FromJSON EventPayload where
  parseJSON = withObject "Payload" $ \v -> Payload <$> v .: "data"
                                                   <*> v .: "eventType"
                                                   <*> v .: "uri"

type URI = String
type Action = String

-- | Queries the LCU for pages and returns either a page named @Anarchy: .*@
-- or the first modifiable page available; 'Nothing' if both are unavailable 
getSuitablePage :: AuthInfo -> MaybeT Req RunePage
getSuitablePage auth = do
    all <- lcuGet "/lol-perks/v1/pages"
    let ours = filter ((=~ ("Anarchy: .*" :: String)) . pageName) all
    case ours of
      []    -> MaybeT . return . safeHead $ filter pageIsEditable all
      (x:_) -> return $ head ours
  where
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    
    lcuGet :: FromJSON a => T.Text -> MaybeT Req a
    lcuGet e =
      MaybeT $ responseBody <$> lcuReq auth GET e NoReqBody jsonResponse mempty

-- | Sets the rune on a runepage returned by 'getSuitablePage' (or creates a
-- new one, if 'Nothing') to the 'Rune' and name @Anarchy: 'champName'@ where
-- @'champName'@ is the name of the 'Champion' returned by a Data Dragon query
setCurrentRune :: AuthInfo -> Champion -> Rune -> IO ()
setCurrentRune auth champ rune = do
    conf <- lcuHttpConfig
    -- we could propagate failure to the callers, for showing something
    -- in the ui or something (?)
    runReq conf . runMaybeT $ do
      champName <- hoist liftIO $
        getChampNameByKey (DDCDN riotCdnUrl "en_US" "11.5.1") champ -- FIXME
      page <- lift . runMaybeT $ getSuitablePage auth
      let name = "Anarchy: " ++ T.unpack champName
      pid <- case page of
               Just target ->
                 lift . reusePage $ target { pageName = name
                                           , pageRune = rune
                                           }
               Nothing ->
                 newPage $ defaultPage { pageName = name
                                       , pageRune = rune
                                       }
      lift $ setCurrentPage pid
    return ()
  where
    setCurrentPage :: Integer -> Req ()
    setCurrentPage pid =
      lcuReq auth PUT "/lol-perks/v1/currentpage" (ReqBodyJson pid)
        bsResponse mempty
      >> return ()

    -- untested
    newPage :: RunePage -> MaybeT Req Integer
    newPage p = do
      page <- lift $ lcuReq auth PUT "/lol-perks/v1/pages"
        (ReqBodyJson p) jsonResponse mempty
      return . pageId $ responseBody page
      
    reusePage :: RunePage -> Req Integer
    reusePage target =
      lcuReq auth PUT ("/lol-perks/v1/pages/" <> (T.pack $ show pid))
        (ReqBodyJson target)
        bsResponse mempty
      >> return pid
      where
        pid = pageId target

-- FIXME take route into consideration (if available)
-- | Attempts to set the ideal runeset for 'Champion' based on answers
-- from a list of 'Provider's, returning the choosen 'Rune' and 'Route'
handleAutorune :: AuthInfo -> Champion -> [Provider] -> IO (Maybe Route, Rune)
handleAutorune auth champ ps = rs >>= \runes -> do
    let (route, rune) = head runes -- FIXME better heuristics for multiple providers
    setCurrentRune auth champ rune
    return (route, rune)
  where
    rs :: IO [(Maybe Route, Rune)]
    rs = let as = sequence $ map (\f -> runMaybeT $ f champ Nothing) ps
             bs = filter (not . null) <$> as
          in map fromJust <$> bs

-- | Receives LCU champ selection events and decides when to call 'handleAutorune',
-- also takes care of notifying the user interface of state changes.
handleChampSelect :: IORef AnarchyConfig
                  -- ^ Contains shared application-wide configuration
                  -> Chan UIMessage
                  -- ^ Communication channel with user interface
                  -> MVar AutoRuneState
                  -- ^ Used internally for synchronization and state-keeping
                  -> AuthInfo
                  -> Object
                  -> IO ()
handleChampSelect confRef ui stateVar auth obj = do
    sequence $ callAutorune <$> champId
    return ()
  where
    champId :: Maybe Champion
    champId = flip parseMaybe obj $ \v -> do
      ourCell  <- v .: "localPlayerCellId" :: Parser Integer
      team     <- v .: "myTeam"
      us       <- head <$> filterM (\o -> (== ourCell) <$> (o .: "cellId")) team
      us .: "championId" :: Parser Champion
  
    callAutorune :: Champion -> IO ()
    callAutorune champ = modifyMVar_ stateVar handleState
      where
        handleState :: AutoRuneState -> IO AutoRuneState
        handleState state = do
          AnarchyConfig { enabledRuneProviders = eProviders
                        } <- readIORef confRef
          case state of
            Unhandled | champ /= 0 -> do
              case sequence $
                map (fmap providerFunction . flip lookup providers) eProviders of
                Nothing -> do
                  writeChan ui $ ARError "Some provider is unknown."
                  return state
                Just ps -> do
                  (route, rune) <- handleAutorune auth champ ps
                  writeChan ui $ PickedRune champ route rune
                  return $ Handled champ
            Handled old | old /= champ -> do
                  handleState Unhandled
            _ -> return state

-- | Listens for LCU WAMP events and calls the appropriate functions
listenForEvents :: [(URI, Maybe Action, Object -> IO ())]
                -- ^ List of functions to be called for events at
                -- specified 'URI' and 'Action'
                -> Connection
                -- ^ Websocket 'Connection' to the LCU
                -> IO ()
listenForEvents ls conn = do
    sendTextData conn ("[5, \"OnJsonApiEvent\"]" :: T.Text)
    forever $ do
      msg <- receiveData conn
      case (decode msg :: Maybe (Int, String, EventPayload)) of
        Just (8, "OnJsonApiEvent", payload) -> dispatchEvent payload
        _ -> return ()
  where
    dispatchEvent :: EventPayload -> IO ()
    dispatchEvent (Payload eData eType eUri) = do
        sequence fs
        return ()
      where
        qualifies (uri, action, _) =
          eUri  == uri &&
          (fromMaybe True $ (== eType) <$> action)
        es = filter qualifies ls
        fs = map (\(_, _, f) -> f eData) es

-- | Attempts to connect to the LCU API and start the autorune system,
-- returns when client disconnects
runAutorune :: IORef AnarchyConfig -> Chan UIMessage -> IO ()
runAutorune confRef uiChan = do
    writeChan uiChan LCUConnecting
    auth <- clientAuth 5000000
    arState <- newMVar Unhandled
    writeChan uiChan LCUConnected
    runLcuWsClient auth $
      listenForEvents [( champSelect
                       , Just "Update"
                       , handleChampSelect confRef uiChan arState auth
                       )
                      ,( champSelect
                       , Just "Delete"
                       , \_ -> do
                           swapMVar arState Unhandled
                           writeChan uiChan OutOfChampSelect
                           return ()
                       )]
  where
    champSelect = "/lol-champ-select/v1/session"
    
    clientAuth :: Int -> IO AuthInfo
    clientAuth delay = do
      r <- runMaybeT getClientAuthInfo
      case r of
        Just auth -> return auth
        Nothing -> do
          writeChan uiChan $ LCUConnectRetry delay
          threadDelay delay
          clientAuth delay
