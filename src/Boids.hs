module Boids ( World
             , Boid(..)
             , initialState
             , renderState
             , nextState
             , radsToDegrees
             ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random
import Control.Monad

data World = World { worldBoids :: [Boid]
                   , worldGuide :: Point
                   }

data Boid = Boid { boidPosition :: Point
                 , boidHeading :: Float
                 , boidSteer :: Float
                 } deriving Show

radsToDegrees :: Float -> Float
radsToDegrees x = x * 180.0 / pi

renderBoid :: Boid -> Picture
renderBoid boid = translate x y $ rotate (-heading) $ polygon ps
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = radsToDegrees $ boidHeading boid
          (x, y) = boidPosition boid


guideBoidTo :: Point -> Boid -> Boid
guideBoidTo (guideX, guideY) boid = boid { boidSteer = da / abs da * 0.25 }
    where (boidX, boidY) = boidPosition boid
          (dx, dy) = (guideX - boidX, guideY - boidY)
          heading = boidHeading boid
          da = atan2 dy dx - boidHeading boid

nextBoid :: Float -> Boid -> Boid
nextBoid deltaTime boid = boid { boidPosition = (x + deltaTime * cos heading * 100.0, y + deltaTime * sin heading * 100.0)
                               , boidHeading = heading + steer * deltaTime
                               }
    where (x, y) = boidPosition boid
          heading = boidHeading boid
          steer = boidSteer boid

nextGuide :: Float -> Point -> Point
nextGuide deltaTime (guideX, guideY) = (guideX + 100.0 * deltaTime, guideY + 100.0 * deltaTime)

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
initialState = do boids <- replicateM 500 randomBoid
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
