module TicTacToeBot where 

import TicTacToeCommon

index = zip [0..]

index2d :: [[a]] -> [(Int, [(Int, a)])]
index2d field = map (\(x,val) -> (x, index val)) (index field)

flattenWithIndex :: [[a]] -> [(Position, a)]
flattenWithIndex field = 
  concat (
    map (\(x, row) -> 
          map (\(y, val) -> 
            ((x,y), val))
            row
    ) 
    (index2d field)
  )

findNextTurn field marker = pos
  where (pos, _) = minmax field marker marker 0 (0, 0)

getEmptyPositionsForField :: Field -> [Position]
getEmptyPositionsForField field = 
  map (\(pos, _) -> pos) (
    filter (\(pos, marker) -> marker == Empty) (flattenWithIndex field)
  )

minmax :: Field -> Marker -> Marker -> Int -> Position -> (Position, Int)
minmax field requiredWinner currentPlayer depth prevPos = do
  let winner = won field
  if winner == None then 
    do 
      let availablePositions = getEmptyPositionsForField field
      let moves = map (\(x, y) -> 
                          minmax 
                            (setMarkerAtPosition field currentPlayer (x, y))
                            requiredWinner 
                            (otherPlayer currentPlayer) 
                            (depth + 1) 
                            (x, y)
                          ) availablePositions
      if requiredWinner == currentPlayer then maxScore moves else minScore moves
  else (prevPos, score winner depth requiredWinner)

score :: Winner -> Int -> Marker -> Int
score winner depth requiredWinner
           | winner == Draw = 0
           | winner == Won requiredWinner = 10 - depth
           | otherwise = depth - 10

maxScore (x:[]) = x
maxScore ((pos, score):xs) =
  if score > oldScore  then (pos, score) else (oldPos, oldScore)
  where (oldPos, oldScore) = maxScore xs

minScore (x:[]) = x
minScore ((pos, score):xs) =
  if score < oldScore then (pos, score) else (oldPos, oldScore)
  where (oldPos, oldScore) = minScore xs
  