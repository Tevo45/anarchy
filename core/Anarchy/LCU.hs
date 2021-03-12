{-# LANGUAGE OverloadedStrings, DataKinds #-}
module Anarchy.LCU
  ( AuthInfo
  , getClientPath
  , parseLockfile
  , authFromLockfile
  , loadClientLockfile
  , getClientAuthInfo
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
import Data.List

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

data AuthInfo = AuthInfo { apiPort :: Int
                         , apiToken :: String
                         } deriving Show

firstMatch :: String -> String -> Maybe String
firstMatch regex str = str =~~ regex >>= (Just . (!!1) . head)

getClientPath :: MaybeT IO String
getClientPath = MaybeT $ firstMatch regex <$> readCreateProcess proc []
  where
    (proc, regex) = case os of
        "mingw32" -> ( shell "wMiC pRoCeSs WhErE nAmE='lEaGuEcLiEnTuX.eXe' GeT cOmMaNdLiNe"
                     , "\"--install-directory=(.+?)\"" :: String
                     )
        "darwin" -> ( shell "ps x -o args | grep LeagueClientUx" -- untested
                    , "--install-directory=(.*?)(?: --|\n|$)"
                    )
        _ -> error $ "One does not simply locate the League client on " ++ os

parseLockfile :: String -> Maybe AuthInfo
parseLockfile s
  | length fields == 5 = Just AuthInfo { apiPort = read $ fields!!2
                                       , apiToken = fields!!3
                                       }
  | otherwise = Nothing
  where
    fields = splitOn ":" s

authFromLockfile :: String -> MaybeT IO AuthInfo
authFromLockfile path = MaybeT $ parseLockfile <$> readFile path

loadClientLockfile :: MaybeT IO AuthInfo
loadClientLockfile = getClientPath >>= authFromLockfile . (</> "lockfile")

-- locating the lockfile with league under wine would be a pain in the ass,
-- so we just parse the command line arguments instead
authFromWineCli :: MaybeT IO AuthInfo
authFromWineCli = MaybeT $ authFromCli <$> readCreateProcess proc []
  where -- FIXME duplicated stuff, could be done better
    proc = shell "ps x -o args | grep LeagueClientUx" -- duplicated
    authFromCli cli = do
      port  <- read <$> firstMatch "--app-port=(.*?)(?: --|\n|$)" cli
      token <- firstMatch "--remoting-auth-token=(.*?)(?: --|\n|$)" cli
      return $ AuthInfo port token

getClientAuthInfo :: MaybeT IO AuthInfo
getClientAuthInfo
  | os == "darwin" || os == "mingw32" = loadClientLockfile
  | otherwise = authFromWineCli -- assume "standard unix with league under wine"

authHeader :: AuthInfo -> Option 'Https
authHeader = basicAuth "riot" . BU.fromString . apiToken

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

runLcuWsClient :: AuthInfo -> ClientApp a -> IO a
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
                              , connectionPort = fromIntegral lcuPort
                              , connectionUseSecure = Just lcuTlsSettings
                              , connectionUseSocks = Nothing
                              }
    opts = defaultConnectionOptions
    -- String/Text/ByteString juggling; there's probably a better way to do this
    authInfo = encodeBase64 . BU.fromString $ "riot:"++(apiToken lf)
    hdrs = [("Authorization", encodeUtf8 $ "Basic " <> authInfo)]
