module Boids (World, initialState, renderState) where

import Graphics.Gloss

type World = (Float, Float)

initialState :: World
initialState = (0.0, 0.0)

renderState :: World -> Picture
renderState (x, y) = translate x y $ circle 20
