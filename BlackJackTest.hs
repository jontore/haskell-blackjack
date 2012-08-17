module BlackJackTest where 
import BlackJack
import Test.HUnit


checkIfAValidHand :: Int -> IO Bool
checkIfAValidHand h = return (h < 26)

testGetHand =  
    TestCase $ do h <- getCard 42
                  a <- checkIfAValidHand h
                  assertBool "Gets a valid black jack hand" $ a

testCalculateHand = 
    TestCase $ let
        v = calculateHand 1 12
        in assertEqual "Check that a blackJack is calculated" 21 v


main = runTestTT $ TestList [testGetHand, testCalculateHand]     
