module BoidsSpec (testGuideBoidToAngle, testZoomControl) where

import Test.HUnit
import Test.HUnit.Approx
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Boids

errorMargin :: Float
errorMargin = 1e-6

fuzzyEquals x y = abs (x - y) <= errorMargin

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

testZoomControl :: Test
testZoomControl =
    TestList [ TestLabel "Wheel Up" wheelUpTestCase
             , TestLabel "Wheel Down" wheelDownTestCase
             , TestLabel "Unrelated event" nonWheelEventTestCase
             ]
    where wheelUpTestCase = generalTestCase wheelUpMessage wheelUpEvent (<)
          wheelDownTestCase = generalTestCase wheelDownMessage wheelDownEvent (>)
          nonWheelEventTestCase = generalTestCase nonWheelEventMessage leftClick fuzzyEquals

          wheelUpMessage = "Expected zoom in on wheel up"
          wheelDownMessage = "Expected zoom out on wheel down"
          nonWheelEventMessage = "Expected no change in zoom on non-wheel event"

          generalTestCase message event predicate =
              let world = emptyState {
                            worldViewPort = viewPortInit {
                                              viewPortScale = initScale
                                            }
                          }
                  initScale = 1.0
                  zoomedWorld = zoomControl event world
                  zoomedScale = viewPortScale $ worldViewPort $ zoomedWorld
              in TestCase $ assertBool message $ predicate initScale zoomedScale

          wheelUpEvent = EventKey (MouseButton WheelUp) Down undefined undefined
          wheelDownEvent = EventKey (MouseButton WheelDown) Down undefined undefined
          leftClick = EventKey (MouseButton LeftButton) undefined undefined undefined
