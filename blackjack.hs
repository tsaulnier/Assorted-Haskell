--Blackjack game given a working Card.hs file. allows user input and evaluates a winner between dealer and player.

import qualified System.Random as R
import qualified Example.Cards as C

main = do
       let wholeDeck = C.fullDeck
       rand <- R.getStdGen
       R.newStdGen
       let shuffledWholeDeck = C.shuffle rand wholeDeck
       let dealerHand = drawHand 2 shuffledWholeDeck
       let playerHand = drawHand 2 $ snd dealerHand
       let playerFirstCard = head . fst $ playerHand
       let playerSecondCard = head . tail . fst $ playerHand
       let dealerCards = fst $ dealerHand
       let dealerDownCard = head $ dealerCards
       let dealerUpCard = head . tail $ dealerCards
       putStrLn ("Your first card is " ++ show playerFirstCard)
       putStrLn ("Your second card is " ++ show playerSecondCard)
       putStrLn ("The dealer's up card is " ++ show dealerUpCard)

       finalPlayerHand <- playerDraw playerHand
       let playerHandEnd = fst finalPlayerHand
       let totalPlayer = totalCards playerHandEnd
       putStrLn ("Your total is " ++ printMaybe totalPlayer)
       putStrLn ("Your final hand is " ++ show playerHandEnd)

       putStrLn ("The dealer's down card is " ++ show dealerDownCard)
       let playerDeckEnd = snd finalPlayerHand

       dealerDraw (dealerCards, playerDeckEnd) totalPlayer

drawHand :: Int -> C.Deck -> ([C.Card],C.Deck)
drawHand n deck = foldl folder ([], deck) [1..n]
                    where
                      folder (hand, deck) _ =
                        let (drawn, nextDeck) = C.draw deck
                          in (drawn:hand, nextDeck)

cardValue :: C.Card -> [Int]
cardValue c
   | C.getRank c == C.Jack = [10]
   | C.getRank c == C.Queen = [10]
   | C.getRank c == C.King = [10]
   | C.getRank c == C.Ace = [1,11]
   | otherwise = [(fromEnum (C.getRank c))+1]

cardValues :: [C.Card] -> [Int]
cardValues [] = [0]
cardValues (x:xs) = (+) <$> (cardValues xs) <*> (cardValue x)

totalCards :: [C.Card] -> Maybe Int
totalCards [] = Just 0
totalCards cards
   | filter (<=21) (cardValues cards) == [] = Nothing
   | otherwise = Just $ maximum $ filter (<=21) $ cardValues cards

playerDraw :: ([C.Card],C.Deck) -> IO (([C.Card],C.Deck))
playerDraw hand = do
   putStrLn ("Hit or Stand?")
   answer <- getLine
   action answer
      where
         action "hit" = do
             let drawHandTemp = drawHand 1 $ snd hand
             let newCard = head . fst $ drawHandTemp
             putStrLn ("Your new card is " ++ show newCard)
             let newHand = (fst hand ++ fst drawHandTemp, snd drawHandTemp)
             let playerTotal = totalCards $ fst newHand
             maybe (return newHand) (const $ playerDraw newHand) playerTotal
         action "stand" = return hand
         action _ = playerDraw hand

dealerDraw :: ([C.Card],C.Deck) -> Maybe Int -> IO()
dealerDraw hand playerTotal = do
   let dealerTotal = totalCards $ fst hand
   if (playerTotal == Nothing)
        then do
        putStrLn("Player busts!")
   else if (dealerTotal == Nothing)
        then do
        putStrLn("Dealer busts!")
   else if (dealerTotal >= playerTotal)
        then do
        putStrLn("Dealer wins " ++ printMaybe dealerTotal ++ " to " ++ printMaybe playerTotal)
   else if (dealerTotal < Just 17)
        then do
        let drawHandTemp = drawHand 1 $ snd hand
        let newCard = head . fst $ drawHandTemp
        putStrLn("Dealer draws " ++ show newCard)
        let newHand = (fst hand ++ fst drawHandTemp, snd drawHandTemp)
        dealerDraw newHand playerTotal
   else putStrLn("Player wins " ++ printMaybe playerTotal ++ " to " ++ printMaybe dealerTotal)

printMaybe :: Show a => Maybe a -> [Char]
printMaybe Nothing = ">21"
printMaybe (Just x) = show x
