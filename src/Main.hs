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

import qualified Network.WebSockets as WS

type State = MVar



application :: MVar Text -> WS.Connection -> IO ()
application state conn = do 
    forever $ do WS.sendTextData conn ("hello world" :: Text)
                 threadDelay (10 ^ 6)

main :: IO ()
main = do
  putStrLn "Init"
  state <- newMVar ("ciao" :: Text)
  WS.runServer "0.0.0.0" 8888 $ application state <=< WS.acceptRequest

