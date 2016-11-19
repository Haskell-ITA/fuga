module Types where

import Data.Map

type Position = (Int, Int)

type Player = String

type Grid = Map Player Position

data Direction = N | S | E | W


