{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Async (race_)
import Control.Concurrent.BroadcastChan as BChan
import Control.Exception (bracket)
import Control.Monad (forever, forM_)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM (atomically, newTVarIO, modifyTVar', readTVarIO, readTVar, TVar, writeTVar)
import System.Random
import Network.WebSockets as WS
import Data.Serialize
import Data.ByteString.Lazy (ByteString)

import Types
import Common

sendState :: BChan.BroadcastChan Out Grid -> WS.Connection -> IO ()
sendState listener conn = forever $ do
  newState <- BChan.readBChan listener
  WS.sendBinaryData conn $ encodeLazy newState

generateUUID :: IO UUID
generateUUID = getStdRandom (randomR (1,100231231321))

position :: IO Int
position = getStdRandom (randomR (1,5))

mkInitialState :: TVar Grid -> IO UUID
mkInitialState state = do
  uuid <- generateUUID
  x <- position
  y <- position
  atomically $ modifyTVar' state (M.insert uuid (x, y))
  return uuid

recvCommands :: BChan.BroadcastChan In Grid -> UUID -> TVar Grid -> WS.Connection -> IO ()
recvCommands bchan uuid state conn = forever $ do
  msg <- receiveData conn :: IO ByteString
  let direction = either (\x -> error ("Invalid message: " ++ x)) id $ decodeLazy msg --MAYBE log the error instead of blowing up
  newGridM <- atomically $ do
    grid <- readTVar state
    let newPos = newPosition direction grid uuid
    if alreadyExists newPos grid
    then return Nothing
    else do
      let updatedGrid = moveUpdate newPos grid uuid
      writeTVar state updatedGrid
      return . Just $ updatedGrid
  forM_ newGridM (BChan.writeBChan bchan)


newPosition :: Direction -> Grid -> UUID -> Position
newPosition dir grid uuid = applyDirection dir pos
  where pos = fromMaybe (error "UUID not found") $ M.lookup uuid grid

alreadyExists :: Position -> Grid -> Bool
alreadyExists newPos = not . M.null . M.filter (== newPos)

moveUpdate :: Position -> Grid -> UUID -> Grid
moveUpdate newPos grid uuid =  M.insert uuid newPos grid

application :: BChan.BroadcastChan In Grid -> TVar Grid -> WS.PendingConnection -> IO ()
application bchan state pending = bracket (addAndNotify bchan state) (removeAndNotify bchan state) $ \uuid -> do
  conn <- WS.acceptRequest pending
  WS.sendBinaryData conn $ encodeLazy uuid
  -- Send initial state since this client still isn't listening to updates
  -- sent to the bchan
  grid <- readTVarIO state
  WS.sendTextData conn $ encodeLazy grid
  listener <- BChan.newBChanListener bchan
  -- Kills both threads when one exits
  race_
    (recvCommands bchan uuid state conn)
    (sendState listener conn)

addAndNotify :: BChan.BroadcastChan In Grid -> TVar Grid -> IO UUID
addAndNotify bchan state = do
  uuid <- mkInitialState state
  grid <- readTVarIO state
  BChan.writeBChan bchan grid
  return uuid

removeAndNotify :: BChan.BroadcastChan In Grid -> TVar Grid -> UUID -> IO ()
removeAndNotify bchan state uuid = do
  grid <- atomically $ do
    modifyTVar' state (M.delete uuid)
    readTVar state
  BChan.writeBChan bchan grid

main :: IO ()
main = do
  putStrLn "Init"
  state <- newTVarIO M.empty
  bchan <- BChan.newBroadcastChan
  WS.runServer "0.0.0.0" 8888 $ application bchan state

