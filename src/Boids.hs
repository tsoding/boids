module Boids ( World
             , Boid(..)
             , initialState
             , renderState
             , nextState
             , getNearbyBoids
             , isWithinViewOf
             ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random
import Control.Monad
import Vector

data World = World { worldBoids :: [Boid]
                   , worldGuide :: Point
                   } deriving Show

data Boid = Boid { boidPosition :: Point
                 , boidHeading :: Float
                 , boidSteer :: Float
                 } deriving (Show, Eq, Ord)

boidsSpeed = 70.0
guideSpeed = 100.0
separationDistance = 10.0
alignmentDistance = 30.0
cohesionDistance = 200.0
viewAngle = pi / 4 * 3

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
    where dx = x2 - x1
          dy = y2 - y1

renderBoid :: Boid -> Picture
renderBoid boid = translate x y $ rotate (-heading) $ pictures [circle (separationDistance / 2.0), polygon ps]
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = radToDeg $ boidHeading boid
          (x, y) = boidPosition boid

guideBoidToAngle :: Float -> Boid -> Boid
guideBoidToAngle angle boid = boid { boidSteer = da / abs da }
    where da = angle - heading
          heading = boidHeading boid

guideBoidToVector :: Vector -> Boid -> Boid
guideBoidToVector direction boid = guideBoidToAngle (argV direction) boid

guideBoidToPoint :: Point -> Boid -> Boid
guideBoidToPoint guidePoint boid = guideBoidToVector direction boid
    where direction = fromPoints (boidPosition boid) guidePoint


isWithinViewOf :: Boid -> Boid -> Bool
isWithinViewOf boid1 boid2 = azimuth <= viewAngle
    where azimuth = angleVV reference (fromPoints (boidPosition boid1) (boidPosition boid2))
          reference = unitVectorAtAngle $ boidHeading boid1

getNearbyBoids :: Boid -> Float -> [Boid] -> [Boid]
getNearbyBoids pivotBoid proximity boids = filter isVisible boids
    where isVisible boid = isCloseEnough boid && isWithinViewOf pivotBoid boid
          isCloseEnough boid = distance (boidPosition pivotBoid) (boidPosition boid) <= proximity

averageBoidsPos :: [Boid] -> Point
averageBoidsPos boids = (sum xs / n, sum ys / n)
    where (xs, ys) = unzip $ map boidPosition boids
          n = fromIntegral $ length boids

averageBoidsHeading :: [Boid] -> Float
averageBoidsHeading boids = (sum $ map boidHeading boids) / n
    where n = fromIntegral $ length boids

separateBoid :: Boid -> [Boid] -> Boid
separateBoid boid otherBoids = case nearBoids of
                                 [] -> boid
                                 _ -> guideBoidToVector escapeDirection boid
    where nearBoids = getNearbyBoids boid separationDistance otherBoids
          escapeDirection = fromPoints (averageBoidsPos nearBoids) (boidPosition boid)

alignBoid :: Boid -> [Boid] -> Boid
alignBoid boid otherBoids = case nearBoids of
                              [] -> boid
                              _ -> guideBoidToAngle targetHeading boid
    where nearBoids = getNearbyBoids boid alignmentDistance otherBoids
          targetHeading = averageBoidsHeading otherBoids

stickBoid :: Boid -> [Boid] -> Boid
stickBoid boid otherBoids = case nearBoids of
                              [] -> boid
                              _ -> guideBoidToPoint targetPoint boid
    where nearBoids = getNearbyBoids boid cohesionDistance otherBoids
          targetPoint = averageBoidsPos nearBoids

boidsProduct :: [Boid] -> (Boid -> [Boid] -> Boid) -> [Boid]
boidsProduct boids f = [ f boid $ excludedBoids i | (i, boid) <- indexedBoids ]
    where indexedBoids = zip [1..] boids
          excludedBoids i = map snd $ filter (\(j, _) -> i /= j) indexedBoids

separationRule :: [Boid] -> [Boid]
separationRule boids = boidsProduct boids separateBoid

alignmentRule :: [Boid] -> [Boid]
alignmentRule boids = boidsProduct boids alignBoid

cohesionRule :: [Boid] -> [Boid]
cohesionRule boids = boidsProduct boids stickBoid

resetBoidsSteer :: [Boid] -> [Boid]
resetBoidsSteer boids = map resetBoid boids
    where resetBoid boid = boid { boidSteer = 0.0 }

nextBoid :: Float -> Boid -> Boid
nextBoid deltaTime boid = boid { boidPosition = ( x + deltaTime * cos heading * boidsSpeed
                                                , y + deltaTime * sin heading * boidsSpeed)
                               , boidHeading = heading + steer * deltaTime
                               }
    where (x, y) = boidPosition boid
          heading = boidHeading boid
          steer = boidSteer boid

nextGuide :: Float -> Point -> Point
nextGuide deltaTime (guideX, guideY) = ( guideX + guideSpeed * deltaTime
                                       , guideY + guideSpeed * deltaTime)

randomBoid :: IO Boid
randomBoid = do x <- randomRIO (-600.0, 600.0)
                y <- randomRIO (-600.0, 600.0)
                heading <- randomRIO (0.0, 2 * pi)
                steer <- randomRIO (0.0, 2 * pi)
                return $ Boid { boidPosition = (x, y)
                              , boidHeading = heading
                              , boidSteer = steer
                              }

initialState :: IO World
initialState = do boids <- replicateM 200 randomBoid
                  return $ World { worldBoids = boids
                                 , worldGuide = (0.0, 0.0)
                                 }

renderState :: World -> Picture
renderState = pictures . map renderBoid . worldBoids

nextState :: ViewPort -> Float -> World -> World
nextState _ deltaTime world = world { worldBoids = boids
                                    , worldGuide = guide
                                    }
    where boids = separationRule $ alignmentRule $ cohesionRule $ resetBoidsSteer $ map (nextBoid deltaTime) $ worldBoids world
          guide = nextGuide deltaTime $ worldGuide world
