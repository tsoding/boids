import Data.List
import Data.Function
import Boids
import Test.HUnit
import System.Exit

import TestDataSpec
import BoidsSpec
import ViewPortTransformSpec
import NavigationSpec

testIsWithinView :: Test
testIsWithinView  = TestCase (assertBool "Seeing a boid behind me" (not $ isWithinViewOf boid1 boid2))
    where boid1 = Boid { boidPosition = (0.0, 0.0)
                       , boidHeading = 0.0
                       , boidSteer = 0.0
                       }
          boid2 = Boid { boidPosition = (-10.0, 0.0)
                       , boidHeading = 0.0
                       , boidSteer = 0.0
                       }

testGetNearbyBoids :: Test
testGetNearbyBoids = TestCase (assertEqual "Got unexpected nearest boids" (sort $ map boidPosition nearestBoids) (sort $ map boidPosition actualResult))
    where pivotBoid = Boid { boidPosition = (388.46451, 289.70023)
                           , boidHeading = -pi / 2.0
                           , boidSteer = 0.0
                           }
          nearestBoids = [ Boid { boidPosition = (291.93515, 228.0032)
                                , boidHeading = 0.0
                                , boidSteer = 0.0
                                }
                         , Boid { boidPosition = (392.85715, 127.36221)
                                , boidHeading = 0.0
                                , boidSteer = 0.0
                                }
                         ]
          farthestBoids = [ Boid { boidPosition = (505.70178, 58.542339)
                               , boidHeading = 0.0
                               , boidSteer = 0.0
                               }

                        , Boid { boidPosition = (404.23859, 333.44159)
                               , boidHeading = 0.0
                               , boidSteer = 0.0
                               }

                        , Boid { boidPosition = (349.0011, 426.28027)
                               , boidHeading = 0.0
                               , boidSteer = 0.0
                               }
                        ]
          allBoids = nearestBoids ++ farthestBoids
          proximity = 203.03714
          actualResult = getNearbyBoids pivotBoid proximity allBoids

main :: IO Counts
main = do results <- runTestTT $ TestList [ TestLabel "Filtering surrounding boids by proximity" testGetNearbyBoids
                                          , TestLabel "Test isWithinViewOf" testIsWithinView
                                          , TestLabel "TestData.getAllBoids" testGetAllBoids
                                          , TestLabel "TestData.getBoidById" testGetBoidById
                                          , TestLabel "TestData.getBoidsGroupById" testGetBoidsGroupById
                                          , TestLabel "Boids.guideBoidToAngle" testGuideBoidToAngle
                                          , TestLabel "Boids.zoomControl" testZoomControl
                                          , TestLabel "ViewPortTransform.zoom" testZoom
                                          ]
          if (errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith (ExitFailure 1)
