{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Map
import GHC.Generics
import Data.Serialize

data Player = Player { playerType :: PlayerType, puuid :: UUID }
  deriving (Ord, Eq, Show, Read, Generic)

data PlayerType = Human | Alien
  deriving (Ord, Eq, Show, Read, Generic)

type UUID = Int

type Position = (Int, Int)

type Grid = Map Player Position

data Direction = N | S | E | W
  deriving (Read, Show, Generic)

data GameMessage = Won | Lost

instance Serialize Direction

instance Serialize Player
instance Serialize PlayerType

