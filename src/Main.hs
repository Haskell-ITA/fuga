{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Control.Concurrent
import Data.Map
import Control.Concurrent.STM

import Network.WebSockets as WS

import Types
import Common

import System.Random
import Data.Monoid


sendState :: UUID -> TVar Grid -> WS.Connection -> IO ()
sendState uuid mvar conn = do
  g <- atomically $ readTVar mvar
  WS.sendTextData conn $ (T.pack $ show g)
  threadDelay 1000000
  sendState uuid mvar conn
  

generateUUID :: IO UUID
generateUUID = getStdRandom (randomR (1,100231231321))

position :: IO Int
position = getStdRandom (randomR (1,5))

mkInitialState :: TVar Grid -> IO UUID
mkInitialState state = do
  uuid <- generateUUID
  x <- position
  y <- position
  atomically $ modifyTVar state (insert uuid (x, y))
  return uuid

recvCommands :: UUID -> TVar Grid -> WS.Connection -> IO ()
recvCommands uuid state conn = do
  msg <- receiveData conn :: IO Text
  join . atomically $ do grid <- readTVar state
                         let direction = read $ T.unpack msg
                         let newPos = newPosition direction grid uuid
                         if alreadyExists newPos grid 
                         then return $ recvCommands uuid state conn
                         else do modifyTVar state (\grid -> moveUpdate newPos grid uuid)
                                 return $ do print $ (T.pack . show) uuid <> " " <> msg
                                             recvCommands uuid state conn

newPosition :: Direction -> Grid -> UUID -> Position
newPosition dir grid uuid = applyDirection dir pos
  where pos = maybe (error "UUID not found") id $ Data.Map.lookup uuid grid

alreadyExists :: Position -> Grid -> Bool
alreadyExists newPos = not . Data.Map.null . Data.Map.filter (== newPos)

moveUpdate :: Position -> Grid -> UUID -> Grid
moveUpdate newPos grid uuid =  insert uuid newPos grid

application :: TVar Grid -> WS.Connection -> IO ()
application state conn = do 
  uuid <- mkInitialState state
  WS.sendTextData conn $ (T.pack $ show uuid)
  forkIO $ recvCommands uuid state conn
  sendState uuid state conn

main :: IO ()
main = do
  putStrLn "Init"
  state <- newTVarIO $ empty
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

