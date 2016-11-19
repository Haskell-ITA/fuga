module Types where

import Data.Map

type UUID = Int

type Position = (Int, Int)

type Grid = Map UUID Position

data Direction = N | S | E | W


