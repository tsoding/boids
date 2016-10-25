import System.Random (newStdGen, randomRs)
import Graphics.Gloss
import Boids

import Utils (saltedRange)

window = InWindow "Boids" (800, 600) (10, 10)

main :: IO ()
main = do gen <- newStdGen
          let salts = randomRs (0 :: Float, 1 :: Float) gen
              boids = initialState 100 salts
          simulate window white 30 boids renderState nextState
