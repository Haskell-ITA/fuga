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

recvCommands :: UUID -> MVar Grid -> WS.Connection -> IO ()
recvCommands uuid state conn = do
  msg <- receiveData conn :: IO Text
  grid <- readMVar state
  let direction = read $ T.unpack msg
  let newPos = newPosition direction grid uuid
  if alreadyExists newPos grid 
  then recvCommands uuid state conn
  else do threadDelay (10 ^ 6 * 10)
          modifyMVar_ state (\grid -> return (moveUpdate newPos grid uuid))
          print $ (T.pack . show) uuid <> " " <> msg
          recvCommands uuid state conn

newPosition :: Direction -> Grid -> UUID -> Position
newPosition dir grid uuid = case dir of N -> (x, y + 1)
                                        S -> (x, y - 1)
                                        E -> (x + 1, y)
                                        W -> (x - 1, y)
  where (x, y) = maybe (error "UUID not found") id $ Data.Map.lookup uuid grid

alreadyExists :: Position -> Grid -> Bool
alreadyExists newPos = not . Data.Map.null . Data.Map.filter (== newPos)

moveUpdate :: Position -> Grid -> UUID -> Grid
moveUpdate newPos grid uuid =  insert uuid newPos grid

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

