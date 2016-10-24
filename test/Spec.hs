import Boids
import Test.HUnit

testInitialState :: Test
testInitialState = TestCase (assertEqual "Initial State" (0.0, 0.0) initialState)

main :: IO Counts
main = runTestTT $ TestList [TestLabel "Test Initial State" testInitialState]
