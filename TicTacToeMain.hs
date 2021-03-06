import Data.Maybe
import TicTacToeCommon
import TicTacToeBot

emptyRow = [Empty, Empty, Empty]
emptyField = [emptyRow, emptyRow, emptyRow]

charToIntX :: Char -> Maybe Int
charToIntX a 
           | a == 'a' = Just 0
           | a == 'b' = Just 1
           | a == 'c' = Just 2
           | otherwise = Nothing

charToIntY :: Char -> Maybe Int
charToIntY a 
           | a == '1' = Just 0
           | a == '2' = Just 1
           | a == '3' = Just 2
           | otherwise = Nothing

getPosition :: IO Position
getPosition = do 
  x <- getChar
  y <- getChar
  putStrLn ""
  let xi = charToIntX x
  let yi = charToIntY y
  if xi /= Nothing && yi /= Nothing then
    return (fromJust xi, fromJust yi)
  else
    do
      putStrLn "Invalid entry, try again:"
      getPosition

getValidPosition :: Field -> IO Position
getValidPosition field = do
  pos <- getPosition
  if isEmpty field pos then 
    return pos
  else
    do
      putStrLn "Position isn't empty."
      getValidPosition field

playGame :: Field -> Marker -> IO ()
playGame field player = do
  let winState = won field
  if winState == None then 
    if player == X then 
      do
        putStrLn "Where do you want to place your mark (e.g. a1)?"
        pos <- getValidPosition field
        let newField = setMarkerAtPosition field X pos
        putStrLn (showField newField) -- TODO: put into function
        playGame newField O
        return ()
    else 
      do
        let pos = findNextTurn field O
        let newField = setMarkerAtPosition field O pos
        putStrLn (showField newField) -- TODO: put into function
        playGame newField X
        return ()
  else if winState == Draw then
    do
      putStrLn "Game ended in a draw!"
      return ()
  else
    do
      putStrLn (show winState ++ " won!")
      return ()

main = do
  let field = emptyField
  putStrLn (showField field) -- TODO: put into function
  playGame field X