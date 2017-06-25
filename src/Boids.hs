module Boids ( World(..)
             , Boid(..)
             , randomState
             , renderState
             , nextState
             , getNearbyBoids
             , isWithinViewOf
             , guideBoidToAngle
             , handleInput
             , emptyState
             ) where

import Data.List
import Data.Maybe
import Data.Fixed
import System.Random
import Control.Monad
import Control.Applicative
import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import ViewPortTransform
import Vector
import Navigation
import Style

data World = World { worldBoids :: [Boid]
                   , worldNavigation :: Navigation
                   , worldMousePosition :: Maybe Point
                   }

data Boid = Boid { boidPosition :: Point
                 , boidHeading :: Float
                 , boidSteer :: Float
                 } deriving (Show, Eq, Ord)

worldSize = 1200.0
enableDebugLayer = False
boidsSpeed = 150.0
guideSpeed = 100.0
separationDistance = 20.0
alignmentDistance = 150.0
cohesionDistance = 150.0
followCursorDistance = 500.0
viewAngle = pi / 6 * 5
steerVelocity = 2.0

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
    where dx = x2 - x1
          dy = y2 - y1

debugViewAngle :: [Picture]
debugViewAngle = [ color debugViewAngleColor $ line [(0.0, 0.0), mulSV 100.0 $ unitVectorAtAngle viewAngle]
                 , color debugViewAngleColor $ line [(0.0, 0.0), mulSV 100.0 $ unitVectorAtAngle (-viewAngle)]
                 ]

debugCircles :: [Picture]
debugCircles = [ color alignmentDebugCircleColor $ circle alignmentDistance
               , color separationDebugCircleColor $ circle separationDistance
               , color cohesionDebugCircleColor $ circle cohesionDistance
               ]

debugLayer :: [Picture]
debugLayer = if enableDebugLayer
             then debugViewAngle ++ debugCircles
             else []

renderBoid :: Boid -> Picture
renderBoid boid = translate x y $ rotate (-heading) $ pictures $ [ color boidColor $ polygon ps ] ++ debugLayer
    where ps = [ (-10.0, 10.0)
               , (20.0, 0.0)
               , (-10.0, -10.0)
               ]
          heading = radToDeg $ boidHeading boid
          (x, y) = boidPosition boid

guideBoidToAngle :: Float -> Boid -> Boid
guideBoidToAngle angle boid
    | abs da >= errorMargin = boid { boidSteer = da / abs da }
    | otherwise = boid
    where da = angle - heading
          heading = boidHeading boid
          errorMargin = 1e-6

guideBoidToVector :: Vector -> Boid -> Boid
guideBoidToVector direction boid = guideBoidToAngle (argV direction) boid

guideBoidToPoint :: Point -> Boid -> Boid
guideBoidToPoint guidePoint boid = guideBoidToVector direction boid
    where direction = fromPoints (boidPosition boid) guidePoint


isWithinViewOf :: Boid -> Point -> Bool
isWithinViewOf watchingBoid observedPoint = azimuth <= viewAngle
    where azimuth = angleVV reference (fromPoints (boidPosition watchingBoid) observedPoint)
          reference = unitVectorAtAngle $ boidHeading watchingBoid


isCloseEnough :: Boid -> Point -> Float -> Bool
isCloseEnough pivotBoid p proximity = distance (boidPosition pivotBoid) p <= proximity

isVisibleFor :: Boid -> Point -> Float -> Bool
isVisibleFor pivotBoid p proximity = isCloseEnough pivotBoid p proximity && isWithinViewOf pivotBoid p

getNearbyBoids :: Boid -> Float -> [Boid] -> [Boid]
getNearbyBoids pivotBoid proximity boids =
    filter (\boid -> isVisibleFor pivotBoid (boidPosition boid) proximity) boids

averageBoidsPos :: [Boid] -> Maybe Point
averageBoidsPos [] = Nothing
averageBoidsPos boids = Just (sum xs / n, sum ys / n)
    where (xs, ys) = unzip $ map boidPosition boids
          n = fromIntegral $ length boids

averageHeading :: [Float] -> Maybe Float
averageHeading [] = Nothing
averageHeading headings = Just $ atan2 (sum (map sin headings) / n) (sum (map cos headings) / n)
    where n = fromIntegral $ length headings

averageBoidsHeading :: [Boid] -> Maybe Float
averageBoidsHeading boids = averageHeading $ map boidHeading boids

separateBoid :: Boid -> [Boid] -> Boid
separateBoid boid otherBoids = fromMaybe boid separatedBoid
    where nearBoids = getNearbyBoids boid separationDistance otherBoids
          separatedBoid = do averagePosition <- averageBoidsPos nearBoids
                             let escapeDirection = fromPoints averagePosition (boidPosition boid)
                             return $ guideBoidToVector escapeDirection boid

alignBoid :: Boid -> [Boid] -> Boid
alignBoid boid otherBoids = fromMaybe boid guidedBoid
    where nearBoids = getNearbyBoids boid alignmentDistance otherBoids
          guidedBoid = do targetHeading <- averageBoidsHeading nearBoids
                          return $ guideBoidToAngle targetHeading boid

stickBoid :: Boid -> [Boid] -> Boid
stickBoid boid otherBoids = fromMaybe boid stickedBoid
    where nearBoids = getNearbyBoids boid cohesionDistance otherBoids
          stickedBoid = do targetPoint <- averageBoidsPos nearBoids
                           return $ guideBoidToPoint targetPoint boid


shyAway :: Boid -> Boid -> Boid
shyAway shyBoid scaryBoid = guideBoidToAngle heading shyBoid
    where heading = argV $ fromPoints (boidPosition scaryBoid) (boidPosition shyBoid)

adjustBoid :: Boid -> [Boid] -> Boid
adjustBoid boid otherBoids = fromMaybe boid
                             $ shiedAwayBoid <|> interestedBoid
    where shiedAwayBoid :: Maybe Boid
          shiedAwayBoid = shyAway boid <$> tooCloseBoid

          tooCloseBoid :: Maybe Boid
          tooCloseBoid = listToMaybe
                         $ getNearbyBoids boid separationDistance otherBoids

          alignmentHeading :: Maybe Float
          alignmentHeading = averageBoidsHeading
                             $ getNearbyBoids boid alignmentDistance otherBoids

          cohesionHeading :: Maybe Float
          cohesionHeading = (\swarmPos -> argV $ fromPoints (boidPosition boid) swarmPos )
                            <$> ( averageBoidsPos
                                $ getNearbyBoids boid cohesionDistance otherBoids
                                )

          interestingDirection :: Maybe Float
          interestingDirection = (liftA2 (\c a -> averageHeading [a, a, a, c]) cohesionHeading alignmentHeading >>= id)
                                 <|> alignmentHeading
                                 <|> cohesionHeading

          interestedBoid :: Maybe Boid
          interestedBoid = (\a -> guideBoidToAngle a boid) <$> interestingDirection

boidsProduct :: [Boid] -> (Boid -> [Boid] -> Boid) -> [Boid]
boidsProduct boids f = [ f boid $ excludedBoids i | (i, boid) <- indexedBoids ]
    where indexedBoids = zip [1..] boids
          excludedBoids i = map snd $ filter (\(j, _) -> i /= j) indexedBoids

separationRule :: [Boid] -> [Boid]
separationRule boids = boidsProduct boids separateBoid

-- TODO(#67): refactor out old rules
alignmentRule :: [Boid] -> [Boid]
alignmentRule boids = boidsProduct boids alignBoid

cohesionRule :: [Boid] -> [Boid]
cohesionRule boids = boidsProduct boids stickBoid

combinedRule :: [Boid] -> [Boid]
combinedRule boids = boidsProduct boids adjustBoid

followPointRule :: Point -> [Boid] -> [Boid]
followPointRule p boids = map (\boid -> if isCloseEnough boid p followCursorDistance
                                        then guideBoidToPoint p boid
                                        else boid)
                          boids

resetBoidsSteer :: [Boid] -> [Boid]
resetBoidsSteer boids = map resetBoid boids
    where resetBoid boid = boid { boidSteer = 0.0 }


worldWrapPoint :: Point -> Point
worldWrapPoint (x, y) = ( mod' (x + worldSize) (2 * worldSize) - worldSize
                        , mod' (y + worldSize) (2 * worldSize) - worldSize)

nextBoid :: Float -> Boid -> Boid
nextBoid deltaTime boid = boid { boidPosition = worldWrapPoint ( x + deltaTime * cos heading * boidsSpeed
                                                               , y + deltaTime * sin heading * boidsSpeed)
                               , boidHeading = atan2 (sin nextHeading) (cos nextHeading)
                               }
    where (x, y) = boidPosition boid
          heading = boidHeading boid
          nextHeading = heading + steerVelocity * steer * deltaTime
          steer = boidSteer boid

nextGuide :: Float -> Point -> Point
nextGuide deltaTime (guideX, guideY) = ( guideX + guideSpeed * deltaTime
                                       , guideY + guideSpeed * deltaTime)

randomBoid :: IO Boid
randomBoid = do x <- randomRIO (-worldSize, worldSize)
                y <- randomRIO (-worldSize, worldSize)
                heading <- randomRIO (0.0, 2 * pi)
                steer <- randomRIO (0.0, 2 * pi)
                return $ Boid { boidPosition = (x, y)
                              , boidHeading = heading
                              , boidSteer = steer
                              }

emptyState :: World
emptyState = World { worldBoids = []
                   , worldNavigation = navigationInit
                   , worldMousePosition = Nothing
                   }

randomState :: IO World
randomState = do boids <- replicateM 300 randomBoid
                 return $ emptyState { worldBoids = boids }

handleInput :: Event -> World -> World
handleInput event world =
    world { worldNavigation = navigationInput event navigation
          , worldMousePosition = case event of
                                   (EventMotion position) -> Just position
                                   _                      -> mousePosition
          }
    where navigation = worldNavigation world
          boids = worldBoids world
          mousePosition = worldMousePosition world

renderState :: World -> Picture
renderState world = applyNavigationToPicture navigation frame
    where frame = pictures $ map renderBoid $ worldBoids world
          navigation = worldNavigation world

-- TODO(#68): recover broken follow cursor feature
nextState :: Float -> World -> World
nextState deltaTime world = world { worldBoids = boids
                                  }
    where boids = combinedRule $ resetBoidsSteer $ map (nextBoid deltaTime) $ worldBoids world
          navigation = worldNavigation world
