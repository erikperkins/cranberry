import Test.HUnit

testTautology :: Test
testTautology = TestCase $ assert True

main :: IO Counts
main = runTestTT $ TestList [testTautology]
