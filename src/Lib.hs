module Lib
    ( someFunc,
    Pitch,
    toPitch,
    feedback,
    test,
    GameState
--    getNotes,
--    initialGuess,
--    nextGuess
    ) where

import Data.List
toPitch :: String -> Maybe Pitch
feedback ::[Pitch] -> [Pitch] -> (Int,Int,Int)
--initialGuess :: ([Pitch],GameState)
--nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)

data Note = A | B | C | D
  deriving (Show,Eq,Ord)
data Octave = O1 | O2 | O3 | O4 | O5
  deriving (Show,Eq,Ord)

data Pitch = Pitch Note Octave
  deriving (Show,Eq)

data Stage = Stage1 | Stage2

data GameState = GameState {
  isInBatchTestStage :: Bool,
  batchTestStage :: [Note],
  singularTestStage :: [Note],
  foundNotes :: [Note]
} deriving (Show)


test =
--  feedback [(Pitch A One)] [(Pitch B Two)]
--   feedback ( [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]) ([(Pitch A O1),(Pitch D O1),(Pitch C O5),(Pitch A O3)])
--    foldl1 Data.List.intersect [[C,D,C], [D,C,C]]
--    getNotes [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]
--    toPitch "R1"
--    isInBatchTestStage (GameState True [] [] [])
  getGuessForState(GameState False [A] [B,C,D] [])
--  addElementThisTimes (Pitch A O2) [Pitch B O3] 10



--initialGuess =
--  ()
--nextGuess (x:xs, beforeGameState) (correctPitches,correctNotes,correctOcts) =
--    if correctPitches == 3
--    then 1-- end todo
--    else
--      getGuessForState (updateState (x:xs,beforeGameState)(correctPitches,correctNotes,correctOcts))
--
--
updateState (x:xs, gameState) (correctPitches,correctNotes,correctOcts)=
  if isInBatchTestStage gameState
  then
    if (correctPitches==0 && correctNotes==0)
    then -- update state -> remove notes from batch array, if empty batch array change stage
      let newArr = filterArray (batchTestStage gameState)(getNotes x:xs) in
        if length newArr == 0
        then (GameState False newArr (singularTestStage gameState) (foundNotes gameState))
        else (GameState True newArr (singularTestStage gameState) (foundNotes gameState))

    else -- update state -> remove notes from batch array and add to singular stage, if empty batch array change stage
      let newArr = filterArray (batchTestStage gameState)(getNotes x:xs) in
        if length newArr == 0
        then (GameState False [] (getNotes x:xs)++singularTestStage (foundNotes gameState))
        else (GameState True [] (getNotes x:xs)++singularTestStage (foundNotes gameState))

  else
    if (correctPitches ==0 && correctNotes ==0 )
    then GameState True [] (filterArray (singularTestStage gameState) (getNotes x:xs)) (foundNotes gameState) -- remove from singular array
    else GameState True [] (filterArray (singularTestStage gameState) (getNotes x:xs)) addElementThisTimes (Note x)
    (foundNotes gameState) (correctNotes+correctPitches)-- remove from singular array add to found notes

getGuessForState gameState =
    if  (isInBatchTestStage gameState)
    then
      if length(batchTestStage gameState) == 1
      then (Pitch (batchTestStage gameState!!0) O1,Pitch (batchTestStage gameState!!0) O1,Pitch (batchTestStage gameState!!0) O1)
      else (Pitch (batchTestStage gameState!!0) O1,Pitch (batchTestStage gameState!!1) O1,Pitch (batchTestStage gameState!!0) O1)
    else
    (Pitch (singularTestStage gameState!!0) O1,Pitch (singularTestStage gameState!!0) O1,Pitch (singularTestStage gameState!!0) O1)


filterArray array notes =
  filter (\(Pitch note oct) -> (elem note notes)) array



addElementThisTimes element list 0 =
  list
addElementThisTimes element list count=
  addElementThisTimes element (element:list) (count-1)









toPitch pitchString
    | (length pitchString /= 2) = Nothing
    | otherwise = Just (Pitch (getNoteFromChar (pitchString !! 0)) (getOctaveFromChar (pitchString !! 1)))
--    todo handle bad characters

getNoteFromChar char
    | char == 'A' = A
    | char == 'B' = A

getOctaveFromChar char
  | char == '1' = O1
  | char == '2' = O2
  | char == '3' = O3

feedback (x:xs) (x1:xs1) =
  feedback_helper (x:xs) (x1:xs1) (0,0,0)

feedback_helper [] [] x=
  x
feedback_helper (x:xs) (x1:xs1) (pitch,note,octave)=
   let (l1,l2,equal_pitches) = pitches (sortBy sortPitch(x:xs)) (sortBy sortPitch(x1:xs1)) 0 in
     (equal_pitches,(notes l1 l2),(octaves l1 l2))

pitches [] [] equal_pitches =
  ([],[],equal_pitches)
pitches (x:xs) (x1:xs1) equal_pitches =
  if x == x1
  then pitches xs xs1 (equal_pitches+1)
  else (x:xs,x1:xs1,equal_pitches)

notes l1 l2  =
    length (foldl1 Data.List.intersect [(getNotes l1), (getNotes l2)])

octaves l1 l2  =
    length (foldl1 Data.List.intersect [(getOctaves l1), (getOctaves l2)])

sortPitch (Pitch note1 oct1) (Pitch note2 oct2)
  | note1 < note2 = LT
  | note1 > note2 = GT
  | note1 == note2 = compare oct1 oct2


getNotes [] =
  []
getNotes ((Pitch note octave):xs) =
  note:(getNotes xs)

getOctaves [] =
  []
getOctaves ((Pitch note octave):xs) =
  octave:(getOctaves xs)


someFunc :: IO ()
someFunc = putStrLn "someFunc"




