{-
4) Write a function corn that accepts the number of ears of corn the customer is purchasing and outputs the total price. The corn is priced according to the following four statements:
If the customer is purchasing less than a dozen ears, the price per ear is $0.50.
If the customer is purchasing 12 to 23 ears, the price per ear is $0.45.
If the customer is purchasing 24 to 35 ears, the price per ear is $0.40.
If the customer is purchasing more than 35 ears, the price per ear is $0.35.
-}

corn :: (Fractional a, Ord a) => a -> a
corn x
   | x < 12 = x * 0.5
   | x < 24 = x * 0.45
   | x < 36 = x * 0.40
   | otherwise = x * 0.35
