module Boids ( World
             , Boid(..)
             , initialState
             , renderState
             , nextState
             , getNearbyBoids
             , isWithinView
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
                 } deriving (Show, Eq)

boidsSpeed = 100.0
guideSpeed = 100.0
separationDistance = 100.0
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


guideBoidToVector :: Vector -> Boid -> Boid
guideBoidToVector direction boid = boid { boidSteer = da / abs da }
    where argument = argV direction
          heading = boidHeading boid
          da = argument - heading

guideBoidToPoint :: Point -> Boid -> Boid
guideBoidToPoint guidePoint boid = guideBoidToVector direction boid
    where direction = fromPoints (boidPosition boid) guidePoint


isWithinView :: Boid -> Boid -> Bool
isWithinView boid1 boid2 = angleVV (unitVectorAtAngle $ boidHeading boid1) (fromPoints (boidPosition boid1) (boidPosition boid2)) <= viewAngle

getNearbyBoids :: Boid -> Float -> [Boid] -> [Boid]
getNearbyBoids pivotBoid nearDistance boids = filter isVisible boids
    where isVisible boid = isCloseEnough boid && isWithinView pivotBoid boid
          isCloseEnough boid = distance (boidPosition pivotBoid) (boidPosition boid) <= nearDistance

averageBoidsPos :: [Boid] -> Point
averageBoidsPos boids = (sum xs / n, sum ys / n)
    where (xs, ys) = unzip $ map boidPosition boids
          n = fromIntegral $ length boids

separateBoid :: Boid -> [Boid] -> Boid
separateBoid boid otherBoids = case nearBoids of
                                 [] -> boid
                                 _ -> guideBoidToVector escapeDirection boid
    where nearBoids = getNearbyBoids boid separationDistance otherBoids
          escapeDirection = fromPoints (averageBoidsPos nearBoids) (boidPosition boid)

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
    where boids = separationRule $ map (guideBoidToPoint guide . nextBoid deltaTime) $ worldBoids world
          guide = nextGuide deltaTime $ worldGuide world
