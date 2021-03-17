{-# LANGUAGE OverloadedStrings, DataKinds #-}
{-|
Module      : Anarchy.LCU
Description : Code for connecting and interacting with the LCU API
Copyright   : (c) Estevan Castilho, 2021
Maintainer  : estevan.cps@gmail.com
Portability : Windows, POSIX

Some parts of this stolen from https://github.com/Pupix/lcu-connector/blob/master/lib/index.js
-}
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
import Data.Proxy

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

-- | LCU authentication information
data AuthInfo = AuthInfo { apiPort :: Int
                         , apiToken :: String
                         } deriving Show

-- | Return first match for regular expression
firstMatch :: String -> String -> Maybe String
firstMatch regex str = ((!!1) . head) <$> str =~~ regex

-- | Attempts to fetch and parse the client location from the command
-- line
--
-- Note that this will return a Win32 path when the LCU is running
-- under wine
getClientPath :: MaybeT IO String
getClientPath = MaybeT $ firstMatch regex <$> readCreateProcess proc []
  where
    (proc, regex) = case os of
        "mingw32" ->
          ( shell "wMiC pRoCeSs WhErE nAmE='lEaGuEcLiEnTuX.eXe' GeT cOmMaNdLiNe"
          , "\"--install-directory=(.+?)\"" :: String
          )
        "darwin" ->
          ( shell "ps x -o args | grep LeagueClientUx" -- untested
          , "--install-directory=(.*?)(?: --|\n|$)"
          )
        _ -> error $ "One does not simply locate the League client on " ++ os

-- | Attempts to serialize the contents of the LCU lockfile into an 'AuthInfo'
parseLockfile :: String -> Maybe AuthInfo
parseLockfile s
  | length fields == 5 = Just AuthInfo { apiPort = read $ fields!!2
                                       , apiToken = fields!!3
                                       }
  | otherwise = Nothing
  where
    fields = splitOn ":" s

-- | Attempts to read and parse 'AuthInfo' from the LCU lockfile
authFromLockfile :: String -> MaybeT IO AuthInfo
authFromLockfile path = MaybeT $ parseLockfile <$> readFile path

-- | Attepts to automatically locate, read and parse 'AuthInfo' from a
-- running LCU
loadClientLockfile :: MaybeT IO AuthInfo
loadClientLockfile = getClientPath >>= authFromLockfile . (</> "lockfile")

-- locating the lockfile with league under wine would be a pain in the ass,
-- so we just parse the command line arguments instead
-- | Attempts to parse 'AuthInfo' directly from the LCU command line,
-- useful for when the client is running under wine since converting
-- the path to a native path would be unportable and cumbersome
authFromWineCli :: MaybeT IO AuthInfo
authFromWineCli = MaybeT $ authFromCli <$> readCreateProcess proc []
  where -- FIXME duplicated stuff, could be done better
    proc = shell "ps x -o args | grep LeagueClientUx" -- duplicated
    authFromCli cli = do
      port  <- read <$> firstMatch "--app-port=(.*?)(?: --|\n|$)" cli
      token <- firstMatch "--remoting-auth-token=(.*?)(?: --|\n|$)" cli
      return $ AuthInfo port token

-- | Attempts to automatically fetch 'AuthInfo' for a currently running
-- LCU instance
getClientAuthInfo :: MaybeT IO AuthInfo
getClientAuthInfo
  | os == "darwin" || os == "mingw32" = loadClientLockfile
  | otherwise = authFromWineCli -- assume "standard unix with league under wine"

-- | Returns an HTTP authorization header from an 'AuthInfo' suitable
-- to use in HTTP requests to the matching LCU instance
authHeader :: AuthInfo -> Option 'Https
authHeader = basicAuth "riot" . BU.fromString . apiToken

-- > mfw type signature longer than function itself
-- | Wrapper for 'req' suitable for making HTTP requests to the LCU
lcuReq :: (HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
           MonadHttp m, HttpMethod method, HttpBody body,
           HttpResponse response)
       => AuthInfo
       -- ^ Corresponding 'AuthInfo' for LCU instance
       -> method
       -> T.Text
       -- ^ Target endpoint for request
       -> body
       -> Proxy response
       -> Option 'Https
       -> m response
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

-- | 'TLSSettings' ignoring self-signed certificates, suitable for making
-- HTTP requests to the LCU
lcuTlsSettings :: TLSSettings
lcuTlsSettings = TLSSettingsSimple { settingDisableCertificateValidation = True
                                   , settingDisableSession = False
                                   , settingUseServerName = False
                                   }

-- | Returns an 'HttpConfig' with options suitable for making HTTP requests
-- to the LCU
lcuHttpConfig :: IO HttpConfig
lcuHttpConfig = do
    lcuManager <- newManager $ mkManagerSettings lcuTlsSettings Nothing
    return $ defaultHttpConfig { httpConfigAltManager = Just lcuManager }

-- | Opens a websocket connection to the LCU and runs the provided 'ClientApp'
runLcuWsClient :: AuthInfo
               -- ^ LCU authentication information
               -> ClientApp a
               -- ^ Client application
               -> IO a
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
