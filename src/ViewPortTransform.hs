module ViewPortTransform (zoom) where

import Graphics.Gloss.Data.ViewPort

zoom :: Float -> ViewPort -> ViewPort
zoom step viewPort = viewPort {
                       viewPortScale = max (scale + step) 0.0
                     }
    where scale = viewPortScale viewPort
