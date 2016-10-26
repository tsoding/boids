import System.Random (newStdGen, randomRs)
import Graphics.Gloss
import Boids

window = InWindow "Boids" (800, 600) (10, 10)

boidsCount = 100 :: Int

main :: IO ()
main = do gen <- newStdGen
          let salts = randomRs (0 :: Float, 1 :: Float) gen
              boids = initialState boidsCount salts
          simulate window white 30 boids renderState nextState
