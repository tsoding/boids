import Boids
import Test.HUnit
import Test.HUnit.Approx
import System.Exit

testRadsToDegrees :: Test
testRadsToDegrees = TestCase (assertApproxEqual "Rads to Degrees" 1e-6 180.0 (radsToDegrees pi))

main :: IO Counts
main = do results <- runTestTT $ TestList [TestLabel "Test Radians to Degrees conversion" testRadsToDegrees]
          if (errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith (ExitFailure 1)

