{-# LANGUAGE OverloadedStrings #-}
module Main where

import LCU

import qualified Data.Text as T
import Data.Maybe

import Network.WebSockets.Connection

import Control.Monad
import Control.Monad.Trans.Maybe

logEvents lf = runLcuWsClient lf $ \conn -> do
    sendTextData conn ("[5, \"OnJsonApiEvent\"]" :: T.Text)
    forever $ do
      msg <- receiveData conn
      print (msg :: T.Text)

main :: IO ()
main = fromJust <$> runMaybeT loadClientLockfile >>= logEvents
