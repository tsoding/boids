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

type World = [Boid]

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

nextBoid :: Float -> Boid -> Boid
nextBoid deltaTime boid = boid {
                            boidPosition = (x + deltaTime * cos heading * 100.0, y + deltaTime * sin heading * 100.0)
                          }
    where (x, y) = boidPosition boid
          heading = boidHeading boid

randomBoid :: IO Boid
randomBoid = do x <- randomRIO (-100.0, 100.0)
                y <- randomRIO (-100.0, 100.0)
                heading <- randomRIO (0.0, 2 * pi)
                return $ Boid { boidPosition = (x, y)
                              , boidHeading = heading
                              , boidSteer = 0.0
                              }

initialState :: IO World
initialState = replicateM 100 randomBoid

renderState :: World -> Picture
renderState = pictures . map renderBoid

nextState :: ViewPort -> Float -> World -> World
nextState _ deltaTime boids = map (nextBoid deltaTime) boids
