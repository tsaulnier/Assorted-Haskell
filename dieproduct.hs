-- will show all the possible products of two dice.

dieProduct :: (a -> b -> c) -> [d] -> [e] -> [(d,e)]
dieProduct f dieOne dieTwo = f <$> dieOne <*> dieTwo
