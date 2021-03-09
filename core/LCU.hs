{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module LCU
  ( getClientPath
  , parseLockfile
  , loadLockfile
  , loadClientLockfile
  , lcuReq
  , lcuHttpConfig
  , runLcuWsClient
  ) where

import Wuss

import System.Process (shell, readCreateProcess)
import System.Info (os)
import System.FilePath

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import Data.Text.Encoding
import Data.ByteString.Base64
import Data.List.Split

import Text.Regex.PCRE

import Network.Connection
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Req
import Network.WebSockets
import Network.WebSockets.Stream

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

data Lockfile = Lockfile { application :: String
                         , pid :: Int
                         , apiPort :: Int
                         , password :: String
                         , protocol :: String
                         } deriving Show

getClientPath :: MaybeT IO String
getClientPath = MaybeT $ pathFromCLI <$> readCreateProcess proc []
  where
    (proc, regex) = case os of
      "mingw32" -> ( shell "wMiC pRoCeSs WhErE nAmE='lEaGuEcLiEnTuX.eXe' GeT cOmMaNdLiNe"
                   , "\"--install-directory=(.+?)\"" :: String
                   )
      "darwin" -> ( shell "ps x -o args | grep LeagueClientUx" -- untested
                  , "--install-directory=(.*?)(?: --|\n|$)"
                  )
      _ -> error $ "Don't know how to locate LoL client on " ++ os

    pathFromCLI :: String -> Maybe String
    pathFromCLI s = s =~~ regex >>= (Just . (!!1) . head)

parseLockfile :: String -> Maybe Lockfile
parseLockfile s -- there might be a better way to write this(?)
  | length fields == 5 = Just Lockfile { application = head fields
                                       , pid = read $ fields!!1
                                       , apiPort = read $ fields!!2
                                       , password = fields!!3
                                       , protocol = fields!!4
                                       }
  | otherwise = Nothing
  where
    fields = splitOn ":" s

loadLockfile :: String -> MaybeT IO Lockfile
loadLockfile path = MaybeT $ parseLockfile <$> readFile path

loadClientLockfile :: MaybeT IO Lockfile
loadClientLockfile = getClientPath >>= loadLockfile . (</> "lockfile")

authHeader :: Lockfile -> Option 'Https
authHeader = basicAuth "riot" . BU.fromString . password

lcuReq lf method endpoint body response extraOpts =
    req method url body response options
  where
    host = https "127.0.0.1"
    path = dropWhile T.null $ T.splitOn "/" endpoint
    url = foldl (/:) host path

    options =
      authHeader lf       <>
      (port $ apiPort lf) <>
      extraOpts

lcuTlsSettings :: TLSSettings
lcuTlsSettings = TLSSettingsSimple { settingDisableCertificateValidation = True
                                   , settingDisableSession = False
                                   , settingUseServerName = False
                                   }

lcuHttpConfig :: IO HttpConfig
lcuHttpConfig = do
    lcuManager <- newManager $ mkManagerSettings lcuTlsSettings Nothing
    return $ defaultHttpConfig { httpConfigAltManager = Just lcuManager }

runLcuWsClient :: Lockfile -> ClientApp a -> IO a
runLcuWsClient lf app = do
    ctx <- initConnectionContext
    conn <- connectTo ctx params
    stream <- makeStream (fmap Just (connectionGetChunk conn))
                         (maybe (return ()) (connectionPut conn . BL.toStrict))
    runClientWithStream stream lcuHost "/" opts hdrs app
  where
    lcuHost = "127.0.0.1"
    lcuPort = apiPort lf
    params = ConnectionParams { connectionHostname = lcuHost
                              , connectionPort = read $ show lcuPort -- ?????
                              , connectionUseSecure = Just lcuTlsSettings
                              , connectionUseSocks = Nothing
                              }
    opts = defaultConnectionOptions
    -- String/Text/ByteString juggling; there's probably a better way to do this
    authInfo = encodeBase64 . BU.fromString $ "riot:"++(password lf)
    hdrs = [("Authorization", encodeUtf8 $ "Basic " <> authInfo)]
