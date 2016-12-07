import Data.List
import Data.Function
import Boids
import Test.HUnit
import System.Exit

testGetNearBoids :: Test
testGetNearBoids = TestCase (assertEqual "Got unexpected nearest boids" (sortBoids nearestBoids) (sortBoids actualResult))
    where pivotBoid = Boid { boidPosition = (388.46451, 289.70023)
                           , boidHeading = 0.0
                           , boidSteer = 0.0
                           }
          nearestBoids = [ Boid { boidPosition = (392.85715, 127.36221)
                                , boidHeading = 0.0
                                , boidSteer = 0.0
                                }
                         , Boid { boidPosition = (480.00003, 389.50507)
                                , boidHeading = 0.0
                                , boidSteer = 0.0
                                }
                         , Boid { boidPosition = (270.71429, 420.21936)
                                , boidHeading = 0.0
                                , boidSteer = 0.0
                                }
                         ]
          farestBoids = [ Boid { boidPosition = (657.22467, 91.877373)
                               , boidHeading = 0.0
                               , boidSteer = 0.0
                               }
                        , Boid { boidPosition = (120.71429, 391.64792)
                               , boidHeading = 0.0
                               , boidSteer = 0.0
                               }
                        ]
          allBoids = nearestBoids ++ farestBoids
          nearDistance = 203.03714
          actualResult = getNearBoids pivotBoid nearDistance allBoids
          sortBoids = sortBy (compare `on` boidPosition)

main :: IO Counts
main = do results <- runTestTT $ TestList [TestLabel "Test filtering nearest boids" testGetNearBoids]
          if (errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith (ExitFailure 1)

