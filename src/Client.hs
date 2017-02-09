module Main where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Control.Concurrent.MVar
import Network.WebSockets
import qualified Data.Text as T
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
                   , wUuid :: UUID }

main :: IO ()
main = do
  grid <- newMVar empty
  let world = World (0,0) grid
  {-withSocketsDo $ -}
  runClient ip port "/" $ app world

app :: (UUID -> World) -> Connection -> IO ()
app world' conn = do
  msg <- receiveData conn :: IO T.Text
  let uuid = read $ T.unpack msg
  let world = world' uuid
  _ <- forkIO $ readUpdateGrid (wGrid world) conn
  playIO (InWindow "fuga" (50, 50) (width, height))
         black
         60
         world
         render
         (handleInput conn)
         step

readUpdateGrid :: MVar Grid -> Connection -> IO ()
readUpdateGrid mgrid conn = do
  msg <- receiveData conn :: IO T.Text
  let grid = read $ T.unpack msg
  _ <- swapMVar mgrid grid
  readUpdateGrid mgrid conn

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
  fromMaybe mempty $ sendTextData conn . T.pack . show <$> direction
  return world --TODO move the player in our world too so we don't need to wait for the server's response
               --     if we do this there's a risk of race conditions here too: TODO use a TVar instead of a MVar

render :: World -> IO Picture
render world = do
  grid <- readMVar $ wGrid world
  return $ render' (wUuid world) grid

render' :: UUID -> Grid -> Picture
render' uuid grid' = foldMap makePlayerMarker grid
                  <> fromMaybe mempty (makeOwnPlayerMarker <$> Map.lookup uuid grid)
                  <> Color white (circleSolid 3) -- just to see the center
                  <> gridLines
  where grid = (\(x, y) -> (fromIntegral x * cellSize, fromIntegral y * cellSize)) <$>  grid'
        (w, h) = (fromIntegral width, fromIntegral height)
        makeOwnPlayerMarker (x, y) = translate x y $ color red $ circleSolid (cellSize/3)
        makePlayerMarker (x, y) = translate x y $ color white $ circle (cellSize/2)
        vertGridLines = foldMap (\n -> line [(n*cellSize,-h/2),(n*cellSize,h/2)]) [-30.5..30] --TODO remove magic number
        horizGridLines = foldMap (\n -> line [(-w/2,n*cellSize),(w/2,n*cellSize)]) [-30.5..30]
        gridLines = color (greyN 0.2) $ vertGridLines <> horizGridLines

