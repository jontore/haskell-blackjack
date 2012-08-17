module BlackJack where
import IO
import Control.Monad
import Control.Monad.ST
import Control.Monad.Random
import System.Random
import Data.Array.ST
import GHC.Arr
import Control.Monad.State
import System.Random
import Debug.Trace

data Card = Card {
    c :: (Int, Char) 
} deriving (Eq, Ord, Show)
data Deck = Deck {
    d :: [Card]
} deriving (Show, Eq, Ord)  

data Game = Game {
    phand :: [Card],
    deck :: Deck
} deriving (Show, Eq, Ord)  

type GameState a = State Game a

setDeck :: Deck -> GameState ()
setDeck deckIn = modify $ \st -> st {deck = deckIn}

setPlayerHand :: [Card] -> GameState ()
setPlayerHand phandIn = modify $ \st -> st {phand = phandIn}

getPlayerHand :: GameState [Card]
getPlayerHand = get >>= \st -> return $ phand st

getDeck :: GameState Deck
getDeck = get >>= \st -> return $ deck st

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

generateDeck :: [Card]
generateDeck = concatMap (\n-> createCard n types) numbers where
    types = ['s', 'h', 'c', 'd']
    numbers = [1..13]

createCard :: Int -> [Char] -> [Card]
createCard n types = map (\elem -> Card elem) $ zip (replicate 4 n) types

getCardValue :: Card -> Int
getCardValue x = fst $ c x

sumHand :: [Card] -> Int
sumHand [] = 0
sumHand (x:[]) = getCardValue x
sumHand (x:y:xs) = calculateValue (getCardValue x) (getCardValue y) + sumHand xs

calculateValue :: Int -> Int -> Int
calculateValue a b 
    | a > 10 && b == 1 = 21
    | a == 1 && b > 10 = 21
    | a == 1 && b < 10 = 11 + b
    | a < 10 && b == 1 = 11 + a
    | a > 10 && b > 10 = 20
    | a > 10 && b < 10 = 10 + b
    | a < 10 && b > 10 = a + 10
    | otherwise = a + b

getCard :: GameState Card
getCard = do
    deck <- getDeck
    let extract (a:xs) = (a, xs)
    let (card, remaining) = extract $ d deck
    setDeck $ Deck remaining
    return $ card

dealHand :: GameState [Card]
dealHand = do 
        a <- getCard
        b <- getCard
        let hand = [a, b]
        setPlayerHand hand
        return $ hand

shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = evalRandIO (shuffle deck)

dealNewCard :: GameState [Card]
dealNewCard = do
                hand <- getPlayerHand 
                newCard <- getCard
                let newHand = newCard:hand
                setPlayerHand newHand
                return $ newHand

dealerPlay :: [Card] -> GameState Int
dealerPlay hand = do
    let value = sumHand hand
    if value < 18
        then do 
            newCard <- getCard
            let newHand = newCard:hand
            dealerPlay newHand
        else
            return $ value

calculateWinner :: (Int, Int) -> String
calculateWinner (player, dealer) 
    | player > 21  = "Dealer"
    | dealer > 21 = "Player"
    | dealer == 21 = "Dealer"
    | player == 21 = "Player"
    | player > dealer = "Player"
    | otherwise = "Dealer"

getPlayerInput :: Int -> IO String
getPlayerInput x 
    | x < 21 = do
        putStrLn "Do you wan't another card?"
        getLine
    | otherwise = return $ show $ "no"

gamePlay :: [Card] -> Game ->  IO ()
gamePlay hand st = do
    let playerValue = sumHand hand
    putStrLn $ "This is your hand " ++ (show hand) ++ " the current value of your hand is " ++ (show playerValue)
    inpStr <- getPlayerInput playerValue
    case inpStr of 
        "yes" -> do 
                    let (newHand, newState) = runState dealNewCard st
                    gamePlay newHand newState
        _ -> do 
                let dealerValue = evalState (dealerPlay []) st
                let winner = calculateWinner (playerValue, dealerValue)
                putStrLn $ "The dealer had: " ++ (show dealerValue)
                putStrLn $ "AND THE WINNER IS ------------" ++ winner ++ "--------------"

main = do 
    let deck = generateDeck
    shuffledDeck <- shuffleDeck deck
    putStrLn "Welcome to Haskell BlackJack"
    let (hand, st) = runState (do 
                        dealHand
                      )
                     $ Game [] $ Deck shuffledDeck
    gamePlay hand st
