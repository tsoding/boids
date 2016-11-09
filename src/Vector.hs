module Vector (fromPoints) where

import Graphics.Gloss

fromPoints :: Point -> Point -> Vector
fromPoints (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
