{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Map
import GHC.Generics
import Data.Serialize

type UUID = Int

type Position = (Int, Int)

type Grid = Map UUID Position

data Direction = N | S | E | W
  deriving (Read, Show, Generic)

instance Serialize Direction

