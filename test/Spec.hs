import Boids
import Test.HUnit

testInitialState :: Test
testInitialState = TestCase (assertEqual "Initial State" (1.0, 0.0) initialState)

main :: IO Counts
main = runTestTT $ TestList [TestLabel "Test Initial State" testInitialState]
