{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map
import Data.Monoid
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Network.WebSockets as WS

import Types
import Common


sendState :: UUID -> TVar Grid -> WS.Connection -> IO ()
sendState uuid mvar conn = do
  g <- atomically $ readTVar mvar
  WS.sendTextData conn $ T.pack $ show g
  threadDelay 100000
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
                         else do modifyTVar state (\grid' -> moveUpdate newPos grid' uuid)
                                 return $ do print $ (T.pack . show) uuid <> " " <> msg
                                             recvCommands uuid state conn

newPosition :: Direction -> Grid -> UUID -> Position
newPosition dir grid uuid = applyDirection dir pos
  where pos = fromMaybe (error "UUID not found") $ Data.Map.lookup uuid grid

alreadyExists :: Position -> Grid -> Bool
alreadyExists newPos = not . Data.Map.null . Data.Map.filter (== newPos)

moveUpdate :: Position -> Grid -> UUID -> Grid
moveUpdate newPos grid uuid =  insert uuid newPos grid

application :: TVar Grid -> WS.Connection -> IO ()
application state conn = do 
  uuid <- mkInitialState state
  WS.sendTextData conn $ T.pack $ show uuid
  _ <- forkIO $ recvCommands uuid state conn
  sendState uuid state conn

main :: IO ()
main = do
  putStrLn "Init"
  state <- newTVarIO empty
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

