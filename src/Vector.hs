module Vector (fromPoints, addTwoVectors) where

import Graphics.Gloss
import Data.List

fromPoints :: Point -> Point -> Vector
fromPoints (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- TODO(dfec87ca-77f5-4ad6-a015-81b49929a5a1): Improve Vector experience
--
-- This application works with vectors pretty extensively, and
-- functions like `addTwoVectors` are not really convenient.
addTwoVectors :: Vector -> Vector -> Vector
addTwoVectors (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)
