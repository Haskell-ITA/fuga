module Types

import Data.Map

type Position = (Int, Int)

type Player = String

type Grid = Map Position Player

data Direction = N | S | E | W