{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Control.Concurrent
import Data.Map

import Network.WebSockets as WS

import Types


sendState :: MVar Grid -> WS.Connection -> IO ()
sendState mvar conn = do
  g <- readMVar mvar
  WS.sendTextData conn $ (T.pack $ show g)
  threadDelay 1000000
  sendState mvar conn

recvCommands :: WS.Connection -> IO ()
recvCommands conn = do
  msg <- receiveData conn :: IO Text
  print msg
  recvCommands conn
  

application :: MVar Grid -> WS.Connection -> IO ()
application state conn = do 
  forkIO $ recvCommands conn
  sendState state conn

main :: IO ()
main = do
  putStrLn "Init"
  state <- newMVar $ empty
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

