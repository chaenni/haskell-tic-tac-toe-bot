import Data.Maybe

data Marker = X | O | Empty

instance Eq Marker where
  X == X = True
  O == O = True
  _ == _ = False

type Row = (Marker, Marker, Marker)
type Field = (Row, Row, Row)

data Winner = None | Won Marker

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

type Position = (Int, Int)

charToIntX :: Char -> Maybe Int
charToIntX a 
           | a == 'a' = Just 1
           | a == 'b' = Just 2
           | a == 'c' = Just 3
           | otherwise = Nothing

charToIntY :: Char -> Maybe Int
charToIntY a 
           | a == '1' = Just 1
           | a == '2' = Just 2
           | a == '3' = Just 3
           | otherwise = Nothing

{-
getPosition = do 
  x <- getChar
  y <- getChar
  xi <- charToIntX x
  yi <- charToIntY y
  if xi \= Nothing && yi \= Nothing then
    return (fromJust xi, fromJust yi)
  else
    do
      putStrLn "Invalid entry, try again:"
      getPosition
      -}