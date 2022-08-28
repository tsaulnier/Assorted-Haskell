--custom star rating structure

data Rating = One | Two | Three | Four | Five deriving (Ord, Eq, Enum)

instance Show Rating where
  show One = "*"
  show Two = "**"
  show Three = "***"
  show Four = "****"
  show Five = "*****"
