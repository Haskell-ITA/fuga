module Main where

import Control.Monad (forever)
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Network.WebSockets
import Data.Flat
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid

import Types
import Common

ip :: String
ip = "127.0.0.1"
port :: Int
port = 8888

width :: Int
width = 800
height :: Int
height = 600
cellSize :: Float
cellSize = 20

data World = World { wDirection :: (Int,Int)
                     -- FIXME ok, ero convinto che ci si potesse muovere in diagonale... (Int,Int) va cambiato in Direction
                   , wGrid :: MVar Grid
                   , wPlayer :: Player }

main :: IO ()
main = do
  grid <- newMVar empty
  let world = World (0,0) grid
  {-withSocketsDo $ -}
  runClient ip port "/" $ app world

app :: (Player -> World) -> Connection -> IO ()
app world' conn = do
  msg <- receiveData conn :: IO ByteString
  let uuid = messageOrError $ unflat msg --MAYBE log the error instead of blowing up
  let world = world' uuid
  _ <- forkIO $ readUpdateGrid (wGrid world) conn
  playIO (InWindow "fuga" (width, height) (50, 50))
         black
         60
         world
         render
         (handleInput conn)
         step

readUpdateGrid :: MVar Grid -> Connection -> IO ()
readUpdateGrid mgrid conn = forever $ do
  msg <- receiveData conn :: IO ByteString
  let grid = messageOrError $ unflat msg --MAYBE log the error instead of blowing up
  _ <- swapMVar mgrid grid
  return ()

step :: Float -> World -> IO World
step = const return -- we don't need to update the world using the time yet. only the inputs are needed.

handleInput :: Connection -> Event -> World -> IO World
handleInput conn event world = do
  -- TODO MAYBE  usare wDirection per permettere di muoversi tenendo premute le frecce?
  -- immagino che qui lens accorcerebbe tutto di un bel po'...
  --let (x,y) = wDirection world
  --let world' = world { wDirection = case event of {-evento per freccia sx giu-} -> (-1,y)
  --                                                {-eccetera-}
  --                                                _ -> (x,y)
  --                   }
  let direction = case event of EventKey (SpecialKey KeyUp)    Down _ _ -> Just N
                                EventKey (SpecialKey KeyDown)  Down _ _ -> Just S
                                EventKey (SpecialKey KeyRight) Down _ _ -> Just E
                                EventKey (SpecialKey KeyLeft)  Down _ _ -> Just W
                                _ -> Nothing
  fromMaybe mempty $ sendBinaryData conn . (flat::Direction->ByteString) <$> direction
  return world --TODO move the player in our world too so we don't need to wait for the server's response
               --     if we do this there's a risk of race conditions here too: TODO use a TVar instead of a MVar

render :: World -> IO Picture
render world = do
  grid <- readMVar $ wGrid world
  return $ render' (wPlayer world) grid

render' :: Player -> Grid -> Picture
render' player grid' = Color white (circleSolid 3) -- just to see the center
                    <> gridLines
                    <> foldMap makePlayerMarker (toList grid)
                    <> fromMaybe mempty (makeOwnPlayerMarker <$> Map.lookup player grid)

  where grid = (\(x, y) -> (fromIntegral x * cellSize, fromIntegral y * cellSize)) <$>  grid'
        (w, h) = (fromIntegral width, fromIntegral height)
        makeOwnPlayerMarker (x, y) = translate x y $ color red $ circleSolid (cellSize/3)
        makePlayerMarker (Player ptype _, (x, y)) = translate x y $ color (playerColor ptype) $ circle (cellSize/2)
        vertGridLines = foldMap (\n -> line [(n*cellSize,-h/2),(n*cellSize,h/2)]) [-30.5..30] --TODO remove magic number
        horizGridLines = foldMap (\n -> line [(-w/2,n*cellSize),(w/2,n*cellSize)]) [-30.5..30]
        gridLines = color (greyN 0.2) $ vertGridLines <> horizGridLines
        playerColor Human = green
        playerColor Alien = red

