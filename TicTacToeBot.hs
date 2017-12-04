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

findNextTurn field marker = fst (maxPossibilities (map (\pos -> (pos, countPossibleWinsForPosition field pos marker marker)) (getEmptyPositionsForField field)))

maxPossibilities :: [(Position, Int)] -> (Position, Int)
maxPossibilities (x:[]) = x 
maxPossibilities ((pos, count):xs) = 
  if count > oldCount then (pos, count) else (oldPos, oldCount)
  where
    (oldPos, oldCount) = maxPossibilities xs 

getEmptyPositionsForField :: Field -> [Position]
getEmptyPositionsForField field = 
  map (\(pos, _) -> pos) (
    filter (\(pos, marker) -> marker == Empty) (flattenWithIndex field)
  )

countPossibleWinsForPosition field pos requiredWinner currentPlayer =
  if winner == None then
    sum(
      map (\pos -> countPossibleWinsForPosition newField pos requiredWinner (otherPlayer currentPlayer) )
          (getEmptyPositionsForField newField)
    )
  else if winner == Draw then 0
  else if getWinner winner == requiredWinner then 1 
  else -1
  where  
    newField = setMarkerAtPosition field currentPlayer pos
    winner = won newField

