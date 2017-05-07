module NavigationSpec (testZoomControl) where

import Test.HUnit
import Test.HUnit.Approx
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Navigation.Internals

errorMargin :: Float
errorMargin = 1e-6

fuzzyEquals x y = abs (x - y) <= errorMargin

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
              -- TODO(1f7e81ed-27de-45b3-8c99-e5ae8d21cf0c): Learn how to apply lenses here
              let navigation = navigationInit {
                                 navigationViewPort = viewPortInit {
                                                        viewPortScale = initScale
                                                      }
                               }
                  initScale = 1.0
                  zoomedNavigation = zoomControl event navigation
                  zoomedScale = viewPortScale $ navigationViewPort $ zoomedNavigation
              in TestCase $ assertBool message $ predicate initScale zoomedScale

          wheelUpEvent = EventKey (MouseButton WheelUp) Down undefined (0.0, 0.0)
          wheelDownEvent = EventKey (MouseButton WheelDown) Down undefined (0.0, 0.0)
          leftClick = EventKey (MouseButton LeftButton) undefined undefined undefined
