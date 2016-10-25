module Boids ( World
             , initialState
             , renderState
             , nextState
             , radsToDegrees
             ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type World = [Boid]

data Boid = Boid { boidPosition :: Point
                 , boidHeading :: Float
                 , boidSteer :: Float
                 }


radsToDegrees :: Float -> Float
radsToDegrees x = x * 180.0 / pi

renderBoid :: Boid -> Picture
renderBoid boid = translate x y $ rotate heading $ polygon ps
    where ps = [ (0.0, 20.0)
               , (-10.0, -10.0)
               , (10.0, -10.0)
               ]
          heading = radsToDegrees $ boidHeading boid
          (x, y) = boidPosition boid

initialState :: World
initialState = [ Boid { boidPosition = (0.0, 0.0)
                      , boidHeading = pi
                      , boidSteer = 0.0
                      }
               ]

renderState :: World -> Picture
renderState = pictures . map renderBoid

nextState :: ViewPort -> Float -> World -> World
nextState _ _ world = world
