import Graphics.Gloss
import Boids

window = InWindow "Boids" (800, 600) (10, 10)

main = do boids <- initialState
          simulate window white 30 boids renderState nextState
