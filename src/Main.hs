import Graphics.Gloss

type World = (Float, Float)

initialState :: World
initialState = (0.0, 0.0)

renderState :: World -> Picture
renderState (x, y) = translate x y $ circle 20.0

window = InWindow "Boids" (800, 600) (10, 10)

main = simulate window white 30 initialState renderState (\_ deltaTime (x, y) -> (x + 10.0 * deltaTime, y + 10.0 * deltaTime))

