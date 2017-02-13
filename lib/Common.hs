module Common where

import Types

applyDirection :: Direction -> Position -> Position
applyDirection N (x,y) = (x, y + 1)
applyDirection S (x,y) = (x, y - 1)
applyDirection E (x,y) = (x + 1, y)
applyDirection W (x,y) = (x - 1, y)

