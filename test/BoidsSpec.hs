module BoidsSpec (testGuideBoidToAngle, testZoom) where

import Test.HUnit
import Test.HUnit.Approx
import Graphics.Gloss.Data.ViewPort
import Boids

errorMargin :: Float
errorMargin = 1e-6

testGuideBoidToAngle :: Test
testGuideBoidToAngle =
    TestList [ TestLabel "Clockwise" clockwiseTestCase
             , TestLabel "Counterclockwise" counterclockwiseTestCase
             , TestLabel "Stable" stableTestCase
             ]
    where clockwiseTestCase = generalTestCase "Expected clockwise steer" 12.0 1.0
          counterclockwiseTestCase = generalTestCase "Expected counterclockwise steer" 9.0 (-1.0)
          stableTestCase = generalTestCase "Expected no steer" 10.0 0.0

          generalTestCase message angle expectedSteer =
              let testBoid = Boid { boidPosition = (0.0, 0.0)
                                  , boidHeading = 10.0
                                  , boidSteer = 0.0
                                  }
                  guidedBoid = guideBoidToAngle angle testBoid
              in TestCase (assertApproxEqual message errorMargin expectedSteer $ boidSteer guidedBoid)

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
