import Boids
import Test.HUnit
import System.Exit

testInitialState :: Test
testInitialState = TestCase (assertEqual "Initial State" (1.0, 0.0) initialState)

main :: IO Counts
main = do results <- runTestTT $ TestList [TestLabel "Test Initial State" testInitialState]
          if (errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith (ExitFailure 1)

