module ViewPortTransformSpec (testZoom) where

import Test.HUnit
import Test.HUnit.Approx
import Graphics.Gloss.Data.ViewPort
import ViewPortTransform

errorMargin :: Float
errorMargin = 1e-6

testZoom :: Test
testZoom = TestList [ TestLabel "Zoom In" zoomInTestCase
                    , TestLabel "Zoom Out" zoomOutTestCase
                    , TestLabel "Zoom Out Saturation" zoomOutSaturationTestCase
                    ]
    where zoomInTestCase = generalTestCase 1.0 0.5 1.5
          zoomOutTestCase = generalTestCase 1.0 (-0.5) 0.5
          zoomOutSaturationTestCase = generalTestCase 0.1 (-0.5) 0.0

          generalTestCase initialScale zoomStep expectedScale =
              let viewPort = viewPortInit { viewPortScale = initialScale }
                  zoomedScale = viewPortScale $ zoom zoomStep viewPort
                  message = "Unexpected result scale"
              in TestCase (assertApproxEqual message errorMargin expectedScale zoomedScale)
