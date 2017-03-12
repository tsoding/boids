import Graphics.Gloss
import Boids

window = InWindow "Boids" (800, 600) (10, 10)

main = do boids <- randomState
          play window white 30 boids renderState handleInput nextState
