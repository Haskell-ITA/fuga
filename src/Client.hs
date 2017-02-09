module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.MVar
import Network.WebSockets
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid

import Types

ip = "127.0.0.1"
port = 8888

width = 800
height = 600
cellSize = 20

data World = World { wDirection :: (Int,Int)
                   , wGrid :: MVar Grid
                   , wUuid :: UUID }

main :: IO ()
main = do
  grid <- newMVar empty
  let world = World (0,0) grid
  {-withSocketsDo $ -}
  runClient ip port "/" $ app world

app world' conn = do
  uuid <- undefined
  let world = world' uuid
  forkIO $ readUpdateGrid (wGrid world) conn
  playIO (InWindow "fuga" (50, 50) (width, height))
         black
         60
         world
         render
         (handleInput conn)
         step

readUpdateGrid :: MVar Grid -> Connection -> IO ()
readUpdateGrid mgrid conn = do
  undefined --TODO leggi i messaggi e aggiorna il mondo
  readUpdateGrid mgrid conn

step = const $ return . id

handleInput conn event = do
  undefined --TODO invia le mosse al server

render :: World -> IO Picture
render world = do
  grid <- readMVar $ wGrid world
  return $ render' (wUuid world) grid

render' :: UUID -> Grid -> Picture
render' uuid grid' = foldMap makePlayerMarker grid
                  <> fromMaybe mempty (makeOwnPlayerMarker <$> Map.lookup uuid grid)
                  <> circleSolid 3 -- just to see the center
  where grid = (\(x, y) -> (fromIntegral x * cellSize, fromIntegral y * cellSize)) <$>  grid'
        makeOwnPlayerMarker (x, y) = translate x y $ color red $ circleSolid (cellSize/3)
        makePlayerMarker (x, y) = translate x y $ circle (cellSize/2)

