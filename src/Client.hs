module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.MVar
import Network.WebSockets

main :: IO ()
main = do
  world <- newMVar empty
  withSocketsDo $ runClient ip port "/" $ app world

app world conn = do
  forkIO $ readUpdateWorld world conn
  playIO (InWindow "fuga" (50, 50) (800, 600))
         black
         60
         world
         render
         (handleInput conn)
         step

readUpdateWorld :: MVar Grid -> Connection -> IO ()
readUpdateWorld mgrid conn = do
  undefined --TODO leggi i messaggi e aggiorna il mondo
  readUpdateWorld mgrid conn

step = return . id

handleInput conn event = do
  undefined --TODO invia le mosse al server

render :: MVar Grid -> IO Picture
render mgrid = do
  grid <- readMVar mgrid
  return $ render' grid

render' :: Grid -> Picture
render grid = undefined --TODO disegna il mondo

