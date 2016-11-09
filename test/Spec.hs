import Boids
import Test.HUnit
import Test.HUnit.Approx
import System.Exit

main :: IO Counts
main = do results <- runTestTT $ TestList []
          if (errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith (ExitFailure 1)

