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

import System.Random

sendState :: UUID -> MVar Grid -> WS.Connection -> IO ()
sendState uuid mvar conn = do
  g <- readMVar mvar
  WS.sendTextData conn $ (T.pack $ show g)
  threadDelay 1000000
  sendState uuid mvar conn

recvCommands :: UUID -> WS.Connection -> IO ()
recvCommands uuid conn = do
  msg <- receiveData conn :: IO Text
  print msg
  recvCommands uuid conn
  

generateUUID :: IO UUID
generateUUID = getStdRandom (randomR (1,100231231321))

position :: IO Int
position = getStdRandom (randomR (1,5))

mkInitialState :: MVar Grid -> IO UUID
mkInitialState state = do
  uuid <- generateUUID
  x <- position
  y <- position
  modifyMVar_ state (return . insert uuid (x, y))
  return uuid

application :: MVar Grid -> WS.Connection -> IO ()
application state conn = do 
  uuid <- mkInitialState state
  WS.sendTextData conn $ (T.pack $ show uuid)
  forkIO $ recvCommands uuid conn
  sendState uuid state conn

main :: IO ()
main = do
  putStrLn "Init"
  state <- newMVar $ empty
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

