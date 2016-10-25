import Boids
import Utils (dosedLists)
import Test.HUnit
import Test.HUnit.Approx
import System.Exit


testRadsToDegrees :: Test
testRadsToDegrees = TestCase (assertApproxEqual "Rads to Degrees" 1e-6 180.0 (radsToDegrees pi))


testDosedLists :: Test
testDosedLists = TestCase $ do
  assertEqual "Dosed by 3 elements" [[1,2,3],[4,5,6],[7,8,9]]
                                    (dosedLists 3 [1..9])
  assertEqual "Dosed by 2 elements" [[1,2],[3,4],[5,6],[7,8]]
                                    (dosedLists 2 [1..8])
  assertEqual "Dosed by 1 element" [[1],[2],[3],[4],[5]]
                                   (dosedLists 1 [1..5])
  assertEqual "Small tails are sliced" [[1,2,3],[4,5,6],[7,8,9]]
                                       (dosedLists 3 [1..10])
  assertEqual "Works well with infinite lists" [[1,2,3],[4,5,6],[7,8,9]]
                                               (take 3 $ dosedLists 3 [1..])



main :: IO Counts
main = do results <- runTestTT tests
          if errors results + failures results == 0
          then exitSuccess
          else exitWith (ExitFailure 1)
  where tests = TestList [ TestLabel "Test Radians to Degrees conversion" testRadsToDegrees
                         , TestLabel "Test dosedLists util function" testDosedLists
                         ]
