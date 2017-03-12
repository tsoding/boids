import Graphics.Gloss
import Boids

window = InWindow "Boids" (800, 600) (10, 10)

main = do boids <- initialState
          play window white 30 boids renderState handleInput nextState
