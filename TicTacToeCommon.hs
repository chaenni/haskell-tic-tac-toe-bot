module TicTacToeCommon where 

data Marker = X | O | Empty

instance Eq Marker where
  X == X = True
  O == O = True
  Empty == Empty = True
  _ == _ = False

instance Show Marker where
  show X = "X"
  show O = "O"
  show Empty = " "

type Row = [Marker]

otherPlayer X = O
otherPlayer O = X

showRow :: Row -> String
showRow [a, b, c] = show a ++ " | " ++ show b ++ " | " ++ show c

type Field = [Row]

separator = "\n  --+---+--\n"

showField :: Field -> String
showField [a, b, c] = 
  "  1   2   3\n" ++
  "a " ++ showRow a ++ separator ++ 
  "b " ++ showRow b ++ separator ++ 
  "c " ++ showRow c

data Winner = None | Won Marker | Draw

getWinner (Won x) = x

instance Eq Winner where
  None == None = True
  Won a == Won b = a == b
  _ == _ = False

instance Show Winner where
  show None = "no one"
  show (Won x) = show x
  show Draw = "draw"

type Position = (Int, Int)
showPos (x, y) = show x ++ show y

markerAtPosition :: Field -> Position -> Marker
markerAtPosition field (x, y) = field !! x !! y

isEmpty :: Field -> Position -> Bool
isEmpty field pos = markerAtPosition field pos == Empty

replaceNth n newVal (x:xs)
                         | n == 0 = newVal:xs
                         | otherwise = x:replaceNth (n-1) newVal xs

setMarkerAtPosition :: Field -> Marker -> Position -> Field
setMarkerAtPosition field marker (x, y) = 
  replaceNth x newRow field
  where
    newRow = replaceNth y marker (field !! x)

allEqNotEmpty :: Marker -> Marker -> Marker -> Bool
allEqNotEmpty a b c = a == b && b == c && a /= Empty

noneEmpty = all ((/=) Empty)

won::Field -> Winner
won [[a1, a2, a3],
     [b1, b2, b3],
     [c1, c2, c3]]
                 | allEqNotEmpty a1 a2 a3 = Won a1
                 | allEqNotEmpty b1 b2 b3 = Won b1
                 | allEqNotEmpty c1 c2 c3 = Won c1
                 | allEqNotEmpty a1 b1 c1 = Won a1
                 | allEqNotEmpty a2 b2 c2 = Won a2
                 | allEqNotEmpty a3 b3 c3 = Won a3
                 | allEqNotEmpty a1 b2 c3 = Won a1
                 | allEqNotEmpty a3 b2 c1 = Won a3
                 | noneEmpty [a1, a2, a3, b1, b2, b3, c1, c2, c3] = Draw
                 | otherwise = None