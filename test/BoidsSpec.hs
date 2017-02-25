module BoidsSpec (testGuideBoidToAngle) where

import Test.HUnit
import Test.HUnit.Approx
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
