import Graphics.Gloss
import Boids
import Style

window = InWindow "Boids" (800, 600) (10, 10)

main = do boids <- randomState
          play window backgroundColor 30 boids renderState handleInput nextState
