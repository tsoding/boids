import Graphics.Gloss
import Boids

window = InWindow "Boids" (800, 600) (10, 10)

main = simulate window white 30 initialState renderState (\_ deltaTime (x, y) -> (x + 10.0 * deltaTime, y + 10.0 * deltaTime))
