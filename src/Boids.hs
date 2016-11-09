module Boids ( World
             , Boid(..)
             , initialState
             , renderState
             , nextState
             ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle
import System.Random
import Control.Monad

data World = World { worldBoids :: [Boid]
                   , worldGuide :: Point
                   } deriving Show

data Boid = Boid { boidPosition :: Point
                 , boidHeading :: Float
                 , boidSteer :: Float
                 } deriving Show

boidsSpeed = 100.0
guideSpeed = 100.0
separationDistance = 50.0

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


guideBoidTo :: Point -> Boid -> Boid
guideBoidTo (guideX, guideY) boid = boid { boidSteer = da / abs da * 0.25 }
    where (boidX, boidY) = boidPosition boid
          (dx, dy) = (guideX - boidX, guideY - boidY)
          heading = boidHeading boid
          da = atan2 dy dx - boidHeading boid

getNearBoids :: Boid -> Float -> [Boid] -> [Boid]
getNearBoids boid boidDistance boids = filter isTooClose boids
    where isTooClose boid' = distance (boidPosition boid) (boidPosition boid') <= boidDistance

averageBoidsPos :: [Boid] -> Point
averageBoidsPos boids = (sum xs / n, sum ys / n)
    where (xs, ys) = unzip $ map boidPosition boids
          n = fromIntegral $ length boids

separateBoid :: Boid -> [Boid] -> Boid
separateBoid boid otherBoids = guideBoidTo guide boid
    where nearBoids = getNearBoids boid separationDistance otherBoids
          guide = averageBoidsPos nearBoids

separationRule :: [Boid] -> [Boid]
separationRule boids = [ separateBoid boid $ excludedBoids i | (i, boid) <- indexedBoids]
    where indexedBoids = zip [1..] boids
          excludedBoids i = map snd $ filter (\(j, _) -> i /= j) indexedBoids

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
randomBoid = do x <- randomRIO (-100.0, 100.0)
                y <- randomRIO (-100.0, 100.0)
                heading <- randomRIO (0.0, 2 * pi)
                steer <- randomRIO (0.0, 2 * pi)
                return $ Boid { boidPosition = (x, y)
                              , boidHeading = heading
                              , boidSteer = steer
                              }

initialState :: IO World
initialState = do boids <- replicateM 100 randomBoid
                  return $ World { worldBoids = boids
                                 , worldGuide = (0.0, 0.0)
                                 }

renderState :: World -> Picture
renderState = pictures . map renderBoid . worldBoids

nextState :: ViewPort -> Float -> World -> World
nextState _ deltaTime world = world { worldBoids = boids
                                    , worldGuide = guide
                                    }
    where boids = map (guideBoidTo guide . nextBoid deltaTime) $ worldBoids world
          guide = nextGuide deltaTime $ worldGuide world
