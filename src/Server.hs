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
import Data.Flat
import Data.ByteString (ByteString)

import Types
import Common

sendState :: BChan.BroadcastChan Out Grid -> WS.Connection -> IO ()
sendState listener conn = forever $ do
  newState <- BChan.readBChan listener
  WS.sendBinaryData conn $ (flat::Grid->ByteString) newState

generateUUID :: IO UUID
generateUUID = getStdRandom (randomR (1,100231231321))

position :: IO Int
position = getStdRandom (randomR (1,5))

--TODO separate player insertion
mkInitialState :: TVar Grid -> IO Player
mkInitialState state = do
  uuid <- generateUUID
  typesList <- fmap playerType . M.keys <$> atomically (readTVar state)
  let aliens = length $ filter (==Alien) typesList
  let humans = length $ filter (==Human) typesList
  let ptype = if aliens < humans then Alien else Human
  x <- position
  y <- position
  let player = Player ptype uuid
  atomically $ modifyTVar' state (M.insert player (x, y))
  return player

recvCommands :: BChan.BroadcastChan In Grid -> Player -> TVar Grid -> WS.Connection -> IO ()
recvCommands bchan player state conn = forever $ do
  msg <- receiveData conn :: IO ByteString
  let direction = messageOrError $ unflat msg --MAYBE log the error instead of blowing up
  newGridM <- atomically $ do
    grid <- readTVar state
    let newPos = newPosition direction grid player
    if alreadyExists newPos grid
    then return Nothing
    else do
      let updatedGrid = moveUpdate newPos grid player
      writeTVar state updatedGrid
      return . Just $ updatedGrid
  forM_ newGridM (BChan.writeBChan bchan)


newPosition :: Direction -> Grid -> Player -> Position
newPosition dir grid player = applyDirection dir pos
  where pos = fromMaybe (error "UUID not found") $ M.lookup player grid

alreadyExists :: Position -> Grid -> Bool
alreadyExists newPos = not . M.null . M.filter (== newPos)

moveUpdate :: Position -> Grid -> Player -> Grid
moveUpdate newPos grid player =  M.insert player newPos grid

application :: BChan.BroadcastChan In Grid -> TVar Grid -> WS.PendingConnection -> IO ()
application bchan state pending = bracket (addAndNotify bchan state) (removeAndNotify bchan state) $ \player -> do
  conn <- WS.acceptRequest pending
  WS.sendBinaryData conn $ (flat::Player->ByteString) player
  -- Send initial state since this client still isn't listening to updates
  -- sent to the bchan
  grid <- readTVarIO state
  WS.sendTextData conn $ (flat::Grid->ByteString) grid
  listener <- BChan.newBChanListener bchan
  -- Kills both threads when one exits
  race_
    (recvCommands bchan player state conn)
    (sendState listener conn)

addAndNotify :: BChan.BroadcastChan In Grid -> TVar Grid -> IO Player
addAndNotify bchan state = do
  player <- mkInitialState state
  grid <- readTVarIO state
  BChan.writeBChan bchan grid
  return player

removeAndNotify :: BChan.BroadcastChan In Grid -> TVar Grid -> Player -> IO ()
removeAndNotify bchan state player = do
  grid <- atomically $ do
    modifyTVar' state (M.delete player)
    readTVar state
  BChan.writeBChan bchan grid

main :: IO ()
main = do
  putStrLn "Init"
  state <- newTVarIO M.empty
  bchan <- BChan.newBroadcastChan
  WS.runServer "0.0.0.0" 8888 $ application bchan state

