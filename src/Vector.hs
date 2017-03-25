module Vector (fromPoints, addTwoVectors) where

import Graphics.Gloss
import Data.List

fromPoints :: Point -> Point -> Vector
fromPoints (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addTwoVectors :: Vector -> Vector -> Vector
addTwoVectors (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)
