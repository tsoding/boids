module ViewPortTransform ( zoom
                         , translateViewPort
                         , zoomWithPivot
                         ) where

import Graphics.Gloss.Data.ViewPort
import Vector

zoom :: Float -> ViewPort -> ViewPort
zoom step viewPort = viewPort {
                       viewPortScale = max (scale + step) 1e-6
                     }
    where scale = viewPortScale viewPort

zoomWithPivot :: Float -> (Float, Float) -> ViewPort -> ViewPort
zoomWithPivot step pivot viewPort = translateViewPort offset $ zoomedViewPort
    where zoomedViewPort = zoom step viewPort
          w1 = invertViewPort viewPort pivot
          w2 = invertViewPort zoomedViewPort pivot
          offset = fromPoints w1 w2

translateViewPort :: (Float, Float) -> ViewPort -> ViewPort
translateViewPort (vecX, vecY) viewPort = viewPort {
                                            viewPortTranslate = ( transX + vecX
                                                                , transY + vecY
                                                                )
                                          }
    where (transX, transY) = viewPortTranslate viewPort
