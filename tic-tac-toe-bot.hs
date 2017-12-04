data Marker = X | O | Empty

instance Eq Marker where
  X == X = True
  O == O = True
  _ == _ = False

type Row = (Marker, Marker, Marker)
type Field = (Row, Row, Row)

data Winner = None | Won Marker

instance Show (Won) where
  show a = a

allEq a b c = a == b && b == c

won::Field -> Winner
won ((a1, a2, a3),
     (b1, b2, b3),
     (c1, c2, c3)) 
                 | allEq a1 a2 a3 = Won a1
                 | allEq b1 b2 b3 = Won b1
                 | allEq c1 c2 c3 = Won c1
                 | allEq a1 b1 c1 = Won a1
                 | allEq a2 b2 c2 = Won a2
                 | allEq a3 b3 c3 = Won a3
                 | allEq a1 b2 c3 = Won a1
                 | allEq a3 b2 c1 = Won a3
                 | otherwise = None