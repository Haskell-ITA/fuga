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
import Data.Monoid


sendState :: UUID -> MVar Grid -> WS.Connection -> IO ()
sendState uuid mvar conn = do
  g <- readMVar mvar
  WS.sendTextData conn $ (T.pack $ show g)
  threadDelay 1000000
  sendState uuid mvar conn

recvCommands :: UUID -> MVar Grid -> WS.Connection -> IO ()
recvCommands uuid grid conn = do
  msg <- receiveData conn :: IO Text
  let direction = read $ T.unpack msg
  modifyMVar_ grid (\grid -> return (moveUpdate direction grid uuid))
  print $ (T.pack . show) uuid <> " " <> msg
  recvCommands uuid grid conn
  

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

move :: Direction -> Grid -> UUID -> Position
move dir grid uuid = case dir of N -> (x, y + 1)
                                 S -> (x, y - 1)
                                 E -> (x + 1, y)
                                 W -> (x - 1, y)
  where (x, y) = maybe (error "UUID not found") id $ Data.Map.lookup uuid grid
  
moveUpdate :: Direction -> Grid -> UUID -> Grid
moveUpdate dir grid uuid = 
  insert uuid (move dir grid uuid) grid

application :: MVar Grid -> WS.Connection -> IO ()
application state conn = do 
  uuid <- mkInitialState state
  WS.sendTextData conn $ (T.pack $ show uuid)
  forkIO $ recvCommands uuid state conn
  sendState uuid state conn

main :: IO ()
main = do
  putStrLn "Init"
  state <- newMVar $ empty
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

