module SlowQueue where

--------------------------------------------------------------
-- Interface 

empty   :: Q a
add     :: a -> Q a -> Q a
remove  :: Q a -> Q a
front   :: Q a -> a
isEmpty :: Q a -> Bool

--------------------------------------------------------------
-- Implementation

data Q a = Q [a]
  deriving (Eq, Show) 

empty             = Q []
add a (Q as)      = Q (as ++ [a])
remove (Q (a:as)) = Q as
front  (Q (a:as)) = a
isEmpty (Q as)    = null as


