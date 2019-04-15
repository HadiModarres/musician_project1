module Proj1
    ( someFunc,
    Pitch,
    toPitch,
    feedback,
    test,
    GameState,
--    getNotes,
    initialGuess,
    nextGuess
    ) where

import Data.List

toPitch :: String -> Maybe Pitch
feedback ::[Pitch] -> [Pitch] -> (Int,Int,Int)
initialGuess :: ([Pitch],GameState)
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)

data Note = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord)
data Octave = O1 | O2 | O3
  deriving (Show,Eq,Ord)

data Pitch = Pitch Note Octave
  deriving (Show,Eq)

data Stage = Stage1 | Stage2

data GameState = GameState {
  isInBatchTestStage :: Bool,
  batchTestStage :: [Note],
  singularTestStage :: [Note],
  foundNotes :: [Note],
  remainingOctaves :: [Octave],
  foundOctaves :: [Octave],
  octavePermutations :: [[Octave]]
} deriving (Show)


test =
--    feedback [Pitch A O1,Pitch C O3,Pitch G O2] [Pitch A O1,Pitch B O1,Pitch A O1]
--    nextGuess ([Pitch A O1,Pitch B O1,Pitch A O1],GameState True [A,B,C,D,E,F,G] [] [] [O1,O2,O3] []) (1,0,0)
--  permutations [O1,O1,O2]
   feedback ( [(Pitch B O2),(Pitch B O2),(Pitch B O2)]) ([(Pitch A O1),(Pitch B O1),(Pitch A O1)])
--  inters (sort [B,B,B]) (sort[A,A,B]) 0

--    inters [D,D,D] [C,D,C]  0
--    getNotes [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]
--    toPitch "R1"
--    isInBatchTestStage (GameState True [] [] [])
--  getGuessForState(GameState True [B,G] [B,C,D] [] [] [O1,O1,O2] [[O1,O1,O2]])

--  addElementThisTimes (Pitch A O2) [Pitch B O3] 10
--    filterArray [Pitch A O1,Pitch C O2,Pitch B O1, Pitch A O3] [A,B]
--  filterNotes [A,B,C] [A,B]
--  updateState ([Pitch B O1,Pitch B O1,Pitch B O1],GameState False [] [A,B] [] [] [O1,O3,O2] []) (0,2,1)
--  getFromRemaingOctaves (GameState True [] [] [] [] [])
--  addToListWithCap O1 [O2,O3,O3] 2 3

filterArray array notes = --tested
  filter (\(Pitch note oct) -> (elem note notes)) array

filterNotes array notes =
  filter (\note -> not(elem note notes)) array


initialGuess =
    ([Pitch A O1,Pitch B O1,Pitch A O1],GameState True [A,B,C,D,E,F] [G] [] [O1,O2,O3] [] [])

nextGuess (x:xs, beforeGameState) (correctPitches,correctNotes,correctOcts) =
   getGuessForState (updateState (x:xs,beforeGameState)(correctPitches,correctNotes,correctOcts))





updateState (x:xs, gameState) (correctPitches,correctNotes,correctOcts)=

  if ((length (foundNotes gameState) == 3)&&(length (foundOctaves gameState) == 3))
  then GameState (isInBatchTestStage gameState) (batchTestStage gameState) (singularTestStage gameState)
                  (foundNotes gameState) (remainingOctaves gameState) (foundOctaves gameState) (permutations (foundOctaves gameState))
  else
      if isInBatchTestStage gameState
      then
        if (correctPitches==0 && correctNotes==0)
        then -- update state -> remove notes from batch array, if empty batch array change stage
          let newArr = filterNotes (batchTestStage gameState) (getNotes (x:xs)) in
            if length newArr == 0
            then (GameState False newArr (singularTestStage gameState) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) [])
            else (GameState True newArr (singularTestStage gameState) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) [])
        else -- update state -> remove notes from batch array and add to singular stage, if empty batch array change stage
          let newArr = filterNotes (batchTestStage gameState)(getNotes (x:xs)) in
            if length newArr == 0
            then (GameState False [] (nub((getNotes (x:xs))++(singularTestStage gameState))) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) [])
            else (GameState True newArr (nub((getNotes (x:xs))++(singularTestStage gameState))) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) [])
      else
        if (correctPitches ==0 && correctNotes ==0 )
        then GameState False [] (filterNotes (singularTestStage gameState) (getNotes (x:xs))) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) []-- remove from singular array
        else GameState False [] (filterNotes (singularTestStage gameState) (getNotes (x:xs))) (addElementThisTimes (getNotes(x:xs)!!0) (foundNotes gameState) (correctNotes+correctPitches)) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) []-- remove from singular array add to found notes

getGuessForState gameState =
  if ((length (foundNotes gameState) == 3)&&(length (foundOctaves gameState) == 3))
  then
    let x:xs = (octavePermutations gameState) in
    ([Pitch ((foundNotes gameState)!!0) (x!!0),Pitch ((foundNotes gameState)!!0) (x!!1), Pitch ((foundNotes gameState)!!0) (x!!2)],
    GameState (isInBatchTestStage gameState) (batchTestStage gameState) (singularTestStage gameState)
                  (foundNotes gameState) (remainingOctaves gameState) (foundOctaves gameState) xs)
  else
    if  (isInBatchTestStage gameState)
    then
      if length(batchTestStage gameState) == 1
      then ([Pitch (batchTestStage gameState!!0) (getFromRemainingOctaves gameState),Pitch (batchTestStage gameState!!0)
            (getFromRemainingOctaves gameState),Pitch (batchTestStage gameState!!0) (getFromRemainingOctaves gameState)],gameState)
      else ([Pitch (batchTestStage gameState!!0) (getFromRemainingOctaves gameState),Pitch (batchTestStage gameState!!1)
          (getFromRemainingOctaves gameState),Pitch (batchTestStage gameState!!0) (getFromRemainingOctaves gameState)],gameState)
    else
    ([Pitch ((singularTestStage gameState)!!0) (getFromRemainingOctaves gameState),Pitch ((singularTestStage gameState)!!0)
      (getFromRemainingOctaves gameState),Pitch ((singularTestStage gameState)!!0) (getFromRemainingOctaves gameState)], gameState)

getFromRemainingOctaves gameState =
  if length(remainingOctaves gameState) == 0
  then O1
  else ((remainingOctaves gameState)!!0)



remove element list = filter (\e -> e/=element) list


addToListWithCap element list 0 cap =
  list
addToListWithCap element list count cap=
  if length list >= cap
  then list
  else addToListWithCap element (element:list) (count-1) cap




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
    | char == 'B' = B
    | char == 'C' = C
    | char == 'D' = D
    | char == 'E' = E
    | char == 'F' = F
    | char == 'G' = G
getOctaveFromChar char
  | char == '1' = O1
  | char == '2' = O2
  | char == '3' = O3




inters target [] count=
  count
inters target (x1:xs1) count=
  if (elem x1 target)
  then inters (delete x1 target) xs1 (count+1)
  else inters target xs1 count



feedback (x:xs) (x1:xs1) =
  feedback_helper (x:xs) (x1:xs1) (0,0,0)

feedback_helper [] [] x=
  x
feedback_helper (x:xs) (x1:xs1) (pitch,note,octave)=
   let (l1,l2,equal_pitches) = pitches (sortBy sortPitch(x:xs)) (sortBy sortPitch(x1:xs1)) 0 in
     (equal_pitches,notes l1 l2,octaves l1 l2)

pitches [] [] equal_pitches =
  ([],[],equal_pitches)
pitches (x:xs) (x1:xs1) equal_pitches =
  if x == x1
  then pitches xs xs1 (equal_pitches+1)
  else (x:xs,x1:xs1,equal_pitches)

notes l1 l2  =
    inters (getNotes l1) (getNotes l2) 0

octaves l1 l2  =
    inters (getOctaves l1) (getOctaves l2) 0

sortPitch (Pitch note1 oct1) (Pitch note2 oct2)
  | note1 < note2 = LT
  | note1 > note2 = GT
  | note1 == note2 = compare oct1 oct2


getNotes [] =
  []
getNotes ((Pitch note octave):xs) =
  note:(getNotes xs)

--getNotesFromFeedback (feedbackPitches,feedbackNotes,feedbackOcts)=


getOctaves [] =
  []
getOctaves ((Pitch note octave):xs) =
  octave:(getOctaves xs)


someFunc :: IO ()
someFunc = putStrLn "someFunc"




