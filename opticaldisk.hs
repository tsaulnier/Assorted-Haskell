-- functions related to an OpticalDisk structure

data OpticalDisk a = CD String String String [String] | DVD String String a

instance (Eq a) => Eq (OpticalDisk a) where
  CD w x y z == CD a b c d = (w, x, y, z) == (a, b, c, d)
  DVD i j k == DVD q r s = (i, j, k) == (q, r, s)

instance (Show a) => Show (OpticalDisk a) where
  show CD w x _ _ = show

  duration :: OpticalDisk -> Duration
  duration (CD _ _ _ []) = (0,0)
  -- duration (CD _ _ _ [x:xs]) = (fst snd x, snd snd x) + duration (CD _ _ _ xs)
  duration (CD _ _ _ d) = (sum [m | (m,_) <-d], sum [s | (_,s) <- d])
  duration (DVD _ _ d) = d
