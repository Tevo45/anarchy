{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}
module Anarchy where

import Anarchy.State
import Anarchy.LCU
import Anarchy.Providers
import Anarchy.Providers.Dummy

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
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar

data ProviderMeta = ProviderMeta { interfaceName :: String
                                 , description :: String
                                 , providerFunction :: Provider
                                 }

instance Show ProviderMeta where
  show ProviderMeta { interfaceName = name
                    , description = desc
                    } = name ++ ": " ++ desc
  

providers :: [(String, ProviderMeta)]
providers = [("dummy", ProviderMeta { interfaceName = "Dummy"
                                    , description = "Useless provider"
                                    , providerFunction = dummyProvider
                                    })]

data EventPayload = Payload { eventData :: Object
                            , eventType :: String
                            , eventUri  :: String
                            } deriving Show

data AutoRuneState = Unhandled | Handled Champion
                   deriving Show

-- TODO we probably don't need all those fields
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

instance FromJSON EventPayload where
  parseJSON = withObject "Payload" $ \v -> Payload <$> v .: "data"
                                                   <*> v .: "eventType"
                                                   <*> v .: "uri"

type URI = String
type Action = String

getSuitablePage :: AuthInfo -> MaybeT Req RunePage
getSuitablePage auth = do
    all <- lcuGet "/lol-perks/v1/pages"
    cur <- lcuGet "/lol-perks/v1/currentpage"
    let ours = filter ((=~ ("Anarchy: .*" :: String)) . pageName) all
    return $ if not $ null ours
      then head ours -- there's already a page named "Anarchy: .*", reuse that
      else cur -- FIXME pick the first available
  where
    lcuGet :: FromJSON a => T.Text -> MaybeT Req a
    lcuGet e =
      MaybeT $ responseBody <$> lcuReq auth GET e NoReqBody jsonResponse mempty

setCurrentRune :: AuthInfo -> Rune -> IO ()
setCurrentRune auth rune = do
    conf <- lcuHttpConfig
    -- we could propagate failure to the callers, for showing something
    -- in the ui or something (?)
    runReq conf . runMaybeT $ do
      target <- getSuitablePage auth
      lift $ do
        lcuReq auth PUT ("/lol-perks/v1/pages/" <> (T.pack . show $ pageId target)) --bad
          (ReqBodyJson target { pageName = "Anarchy: Haskell"
                              , pageRune = rune
                              })
          bsResponse mempty
        lcuReq auth PUT "/lol-perks/v1/currentpage" (ReqBodyJson $ pageId target)
          bsResponse mempty
    return ()

handleAutorune :: AuthInfo -> Champion -> [Provider] -> IO (Maybe Route, Rune)
handleAutorune auth champ ps = rs >>= \runes -> do
    let (route, rune) = head runes -- FIXME
    setCurrentRune auth rune
    return (route, rune)
  where
    rs :: IO [(Maybe Route, Rune)]
    rs = let as = sequence $ map (\f -> runMaybeT $ f champ Nothing) ps
             bs = filter (not . null) <$> as
          in map fromJust <$> bs

handleChampSelect :: IORef AnarchyConfig
                  -> MVar AutoRuneState
                  -> AuthInfo
                  -> Object
                  -> IO ()
handleChampSelect confRef stateVar auth obj = do
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
                Nothing -> do -- One of the providers is unknown
                  putStrLn "Some provider is unknown." -- TODO raise condition on UI
                  return state
                Just ps -> do
                  handleAutorune auth champ ps -- TODO UI notification
                  return $ Handled champ
            Handled old | old /= champ -> do
                  handleState Unhandled
            _ -> return state

listenForEvents :: [(URI, Maybe Action, Object -> IO ())] -> Connection -> IO ()
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
        qualifies (uri, action, _) = eUri  == uri &&
                                     (fromMaybe True $ (== eType) <$> action)
        es = filter qualifies ls
        fs = map (\(_, _, f) -> f eData) es
        

runAutorune :: IORef AnarchyConfig -> IO ()
runAutorune confRef = do
    putStrLn "Trying my best."
    auth <- clientAuth
    arState <- newMVar Unhandled
    putStrLn "Almost there!"
    runLcuWsClient auth $ listenForEvents [( champSelect
                                           , Just "Update"
                                           , handleChampSelect confRef arState auth
                                           )
                                          ,( champSelect
                                           , Just "Delete"
                                           , \_ -> do
                                               swapMVar arState Unhandled
                                               return ()
                                           )]
  where
    champSelect = "/lol-champ-select/v1/session"
    
    clientAuth :: IO AuthInfo
    clientAuth = do
      r <- runMaybeT getClientAuthInfo
      case r of
        Just auth -> return auth
        Nothing -> do
          threadDelay 5000000
          clientAuth
