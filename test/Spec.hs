import Boids
import Utils (saltedRange)
import Test.HUnit
import Test.HUnit.Approx
import System.Exit


testRadsToDegrees :: Test
testRadsToDegrees = TestCase (assertApproxEqual "Rads to Degrees" 1e-6 180.0 (radsToDegrees pi))


testSaltedRange :: Test
testSaltedRange = TestCase $ do
  assertEqual "Middle" 30 (saltedRange 0.5 (20, 40))
  assertEqual "Minimum" 20 (saltedRange 0 (20, 40))
  assertEqual "Maximum" 40 (saltedRange 1 (20, 40))


main :: IO Counts
main = do results <- runTestTT tests
          if errors results + failures results == 0
          then exitSuccess
          else exitFailure
  where tests = TestList [ TestLabel "Test Radians to Degrees conversion" testRadsToDegrees
                         , TestLabel "Test saltedRange util function" testSaltedRange
                         ]
