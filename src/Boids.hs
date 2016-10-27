module Boids ( World
             , Boid(..)
             , initialState
             , renderState
             , nextState
             , radsToDegrees
             ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Utils (saltedRange)

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
nextBoid deltaTime boid = boid { boidPosition = (nextPos x cos, nextPos y sin) }
    where (x, y) = boidPosition boid
          heading = boidHeading boid
          nextPos axis f = axis + deltaTime * (f heading) * 100.0

randomBoid :: (Float, Float, Float) -> Boid
randomBoid salts = Boid { boidPosition = (getPos s1, getPos s2)
                        , boidHeading = saltedRange s3 (0, 2 * pi)
                        , boidSteer = 0.0
                        }
  where (s1, s2, s3) = salts
        getPos s = saltedRange s (-100, 100)

initialState :: Int -> [Float] -> World
initialState 0          _                = []
initialState boidsCount (s1:s2:s3:salts) = randomBoid (s1, s2, s3)
                                         : initialState (boidsCount - 1) salts

renderState :: World -> Picture
renderState = pictures . map renderBoid

nextState :: ViewPort -> Float -> World -> World
nextState _ deltaTime boids = map (nextBoid deltaTime) boids
