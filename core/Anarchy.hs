{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Anarchy where

import Anarchy.LCU
import Anarchy.Providers
import Anarchy.Providers.Dummy

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

import Network.WebSockets.Connection

import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Exception

data ProviderMeta = ProviderMeta { interfaceName :: String
                                 , description :: String
                                 , fn :: Provider
                                 }

instance Show ProviderMeta where
  show ProviderMeta { interfaceName = name
                    , description = desc
                    } = name ++ ": " ++ desc
  

providers :: [(String, ProviderMeta)]
providers = [("dummy", ProviderMeta { interfaceName = "Dummy"
                                    , description = "Useless provider"
                                    , fn = dummyProvider
                                    })]

data EventPayload = Payload { eventData :: Object
                            , eventType :: String
                            , eventUri  :: String
                            } deriving Show

instance FromJSON EventPayload where
  parseJSON = withObject "Payload" $ \v -> Payload <$> v .: "data"
                                                   <*> v .: "eventType"
                                                   <*> v .: "uri"

type URI = String
type Action = String

handleAutorune :: Object -> IO ()
handleAutorune _ = putStrLn "sweeeeet!"

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
        

runAutorune :: IO ()
runAutorune = do
    putStrLn "Trying my best."
    lf <- clientAuth
    putStrLn "Almost there!"
    runLcuWsClient lf $ listenForEvents [( "/lol-champ-select/v1/session"
                                         , Just "Update"
                                         , handleAutorune
                                         )]
  where
    clientAuth :: IO AuthInfo
    clientAuth = do
      r <- runMaybeT getClientAuthInfo
      case r of
        Just lf -> return lf
        Nothing -> do
          threadDelay 5000000
          clientAuth

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
