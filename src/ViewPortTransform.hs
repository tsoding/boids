module ViewPortTransform (zoom, translateViewPort) where

import Graphics.Gloss.Data.ViewPort

zoom :: Float -> ViewPort -> ViewPort
zoom step viewPort = viewPort {
                       viewPortScale = max (scale + step) 1e-6
                     }
    where scale = viewPortScale viewPort

translateViewPort :: (Float, Float) -> ViewPort -> ViewPort
translateViewPort (vecX, vecY) viewPort = viewPort {
                                            viewPortTranslate = ( transX + vecX
                                                                , transY + vecY
                                                                )
                                          }
    where (transX, transY) = viewPortTranslate viewPort
