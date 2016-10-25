module Boids ( World
             , Boid(..)
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
                 } deriving Show

-- dosedLists 2 [1..6] = [[1,2],[3,4],[5,6]]
-- dosedLists 3 [1..6] = [[1,2,3],[4,5,6]]
dosedLists :: (Num a) => Int -> [a] -> [[a]]
dosedLists doseSize list = dose list []
  where dose list result
          | length taken < doseSize = result
          | otherwise = taken : dose (drop doseSize list) result
            where taken = take doseSize list

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
nextBoid deltaTime boid = boid { boidPosition = (pos x cos, pos y sin) }
    where (x, y) = boidPosition boid
          heading = boidHeading boid
          pos axis f = axis + deltaTime * (f heading) * 100.0

randomBoid :: [Float] -> Boid
randomBoid salts = Boid { boidPosition = (getPos s1, getPos s2)
                        , boidHeading = range s2 (0, 2 * pi)
                        , boidSteer = 0.0
                        }
  where [s1, s2, s3] = salts
        range :: Float -> (Float, Float) -> Float
        range s (from, to) = s * ((to + 1) - from) + from
        getPos :: Float -> Float
        getPos s = range s (-100, 100)

initialState :: Int -> [Float] -> World
initialState boidsCount salts = [ randomBoid (dosedLists 3 salts !! i)
                                | i <- [0..boidsCount-1] ]

renderState :: World -> Picture
renderState = pictures . map renderBoid

nextState :: ViewPort -> Float -> World -> World
nextState _ deltaTime boids = map (nextBoid deltaTime) boids
