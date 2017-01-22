module TestDataSpec ( testGetAllBoids
                    , testGetBoidById
                    , testGetBoidsGroupById
                    ) where

import Test.HUnit
import Test.HUnit.Approx
import TestData
import Boids
import Text.XML.Light.Input
import Data.List
import Data.Function
import Data.Maybe

errorMargin :: Float
errorMargin = 1e-6

assertBoidsListsEqual :: [Boid] -> [Boid] -> Assertion
assertBoidsListsEqual expectedBoids actualBoids = sequence_ assertions
    where expectedBoids' = sort expectedBoids
          actualBoids' = sort actualBoids
          lengthEquality = assertEqual "" (length expectedBoids) (length actualBoids)
          assertions = lengthEquality : (map (uncurry $ assertBoidsEqual) $ zip expectedBoids' actualBoids')

assertBoidsEqual :: Boid -> Boid -> Assertion
assertBoidsEqual expectedBoid actualBoid = sequence_ assertions
    where
      properties = [ (fst . boidPosition, "X coordinates were not equal")
                   , (snd . boidPosition, "Y coordinates were not equal")
                   , (boidHeading, "Headings were not equal")
                   , (boidSteer, "Steers were not equal") ]
      assertions = map assertBoidsEqual properties
      assertBoidsEqual (f, message) = assertApproxEqual message errorMargin (f expectedBoid) (f actualBoid)

boidsEqualTest :: Boid -> Boid -> Test
boidsEqualTest expectedBoid actualBoid = TestLabel message $ TestList assertions
    where
      message = concat ["Expected: "
                       , show expectedBoid
                       , ", Actual: "
                       , show actualBoid
                       ]
      properties = [ (fst . boidPosition, "X coordinate")
                   , (snd . boidPosition, "Y coordinate")
                   , (boidHeading, "Heading")
                   , (boidSteer, "Steer") ]
      assertions = map assertBoidsEqual properties
      assertBoidsEqual (f, message) = TestCase (assertApproxEqual message errorMargin (f expectedBoid) (f actualBoid))


testGetAllBoids :: Test
testGetAllBoids = TestList $ map (uncurry $ boidsEqualTest) $ zip allBoids expectedBoids
    where xmlData = unlines [ "<svg xmlns='http://www.w3.org/2000/svg'>"
                            , "  <circle cx='349.0011' cy='426.28027' />"
                            , "  <g>"
                            , "    <circle cx='356.3743' cy='234.00' />"
                            , "    <circle cx='123.459' cy='668.46458' />"
                            , "  </g>"
                            , "  <circle cx='972.374' cy='33.34923' />"
                            , "</svg>"
                            ]

          xmlRoot = parseXMLDoc xmlData

          expectedBoids = [ Boid {boidPosition = (349.0011,426.28027), boidHeading = 0.0, boidSteer = 0.0}
                          , Boid {boidPosition = (356.3743,234.0), boidHeading = 0.0, boidSteer = 0.0}
                          , Boid {boidPosition = (123.459,668.4646), boidHeading = 0.0, boidSteer = 0.0}
                          , Boid {boidPosition = (972.374,33.34923), boidHeading = 0.0, boidSteer = 0.0}
                          ]

          allBoids = getAllBoids xmlRoot


testGetBoidById :: Test
testGetBoidById = TestList [ fromMaybe (TestCase $ assertFailure "Could not find boid with id 'pivot'") positiveTestCase
                           , negativeTestCase
                           ]
    where expectedBoid = Boid {boidPosition = (349.0011,426.28027), boidHeading = 0.0, boidSteer = 0.0}
          xmlData = unlines [ "<svg xmlns='http://www.w3.org/2000/svg'>"
                            , "  <circle id='pivot' cx='349.0011' cy='426.28027' />"
                            , "  <circle cx='972.374' cy='33.34923' />"
                            , "</svg>" ]
          xmlRoot = parseXMLDoc xmlData
          positiveTestCase = do actualBoid <- getBoidById xmlRoot "pivot"
                                return $ TestList [boidsEqualTest expectedBoid actualBoid]
          negativeTestCase = TestCase (assertBool "Found non-existing element" $ isNothing $ getBoidById xmlRoot "khooy")

testGetBoidsGroupById :: Test
testGetBoidsGroupById = TestList [ nonExistingIdCase
                                 , innerGroupCase
                                 , outerGroupCase ]
    where xmlData = unlines [ "<svg>"
                            , "  <g id='outer'>"
                            , "    <circle cx='326' cy='155' />"
                            , "    <circle cx='478' cy='419' />"
                            , "    <circle cx='107' cy='449' />"
                            , "    <g id='inner'>"
                            , "      <circle cx='102' cy='152' />"
                            , "      <circle cx='327' cy='246' />"
                            , "      <circle cx='444' cy='358' />"
                            , "    </g>"
                            , "  </g>"
                            , "</svg>"
                            ]
          xmlRoot = parseXMLDoc xmlData

          nonExistingIdCase = TestCase (assertBool "Found non-existing elements" $ null $ getBoidsGroupById xmlRoot "blah")
          innerGroupCase = TestCase (assertBoidsListsEqual expectedInnerBoids actualInnerBoids)
              where
                expectedInnerBoids = [ Boid { boidPosition = (102, 152), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (327, 246), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (444, 358), boidHeading = 0.0, boidSteer = 0.0 }
                                     ]
                actualInnerBoids = getBoidsGroupById xmlRoot "inner"
          outerGroupCase = TestCase (assertBoidsListsEqual expectedOuterBoids actualOuterBoids)
              where
                expectedOuterBoids = [ Boid { boidPosition = (102, 152), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (327, 246), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (444, 358), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (326, 155), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (478, 419), boidHeading = 0.0, boidSteer = 0.0 }
                                     , Boid { boidPosition = (107, 449), boidHeading = 0.0, boidSteer = 0.0 }
                                     ]
                actualOuterBoids = getBoidsGroupById xmlRoot "outer"
