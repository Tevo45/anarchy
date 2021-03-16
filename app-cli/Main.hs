{-# LANGUAGE OverloadedStrings #-}
module Main where

import Anarchy
import Anarchy.State

import Data.IORef
import Data.Maybe

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad

-- TODO command line arguments, etc etc
main = do
    conf <- newIORef $ AnarchyConfig ["op.gg", "dummy"]
    chan <- newChan
    forkIO . forever $ runAutorune conf chan
    forever $ do
        msg <- readChan chan
        case msg of
          LCUConnecting -> putStrLn "Trying my best."
          LCUConnectRetry n ->
            putStrLn $ "Connection failed, trying again in " ++
                       (show $ n `div` 1000000) ++ "s"
          LCUConnected -> putStrLn "Almost there!"
          ARError e -> putStrLn $ "Error: "++e
          PickedRune champ _ rune ->
            putStrLn $ "Picked rune " ++ show rune ++ " for champ " ++ show champ ++ "."
          OutOfChampSelect -> putStrLn "Out of champ selection screen."
