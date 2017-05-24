{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Types where

import Data.Map
import GHC.Generics
import Data.Flat

data Player = Player { playerType :: PlayerType, puuid :: UUID }
  deriving (Ord, Eq, Show, Read, Generic, Flat)

data PlayerType = Human | Alien
  deriving (Ord, Eq, Show, Read, Generic, Flat)

type UUID = Int

type Position = (Int, Int)

type Grid = Map Player Position

data Direction = N | S | E | W
  deriving (Read, Show, Generic, Flat)

data GameMessage = Won | Lost

