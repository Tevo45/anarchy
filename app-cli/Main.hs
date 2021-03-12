{-# LANGUAGE OverloadedStrings #-}
module Main where

import LCU

import Anarchy

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe

import Network.WebSockets.Connection

import Control.Monad
import Control.Monad.Trans.Maybe

logEvents lf = runLcuWsClient lf $ \conn -> do
    sendTextData conn ("[5, \"OnJsonApiEvent\"]" :: T.Text)
    forever $ do
      msg <- receiveData conn
      TIO.putStrLn $ (msg :: T.Text)

main :: IO ()
main = runAutorune
--main = fromJust <$> runMaybeT loadClientLockfile >>= logEvents
