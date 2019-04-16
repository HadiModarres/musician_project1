module Proj1
    (
    Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess
    ) where

import Data.List

toPitch :: String -> Maybe Pitch
feedback ::[Pitch] -> [Pitch] -> (Int,Int,Int)
initialGuess :: ([Pitch],GameState)
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)

data Pitch = Pitch Note Octave
  deriving (Show,Eq)


--how guessing logic works: we have 3 stages in the game, first stage is the batch stage in which
--we guess notes in batches to see if they exist in the target, after that they either get added to singular stage
--or not. those that were added to singular test stage get test individually and the found notes are added to found notes
--octaves are test in 3 steps separately. when we find 3 correct notes and 3 correct octaves we try different permutations
--until we find the correct chord

data GameState = GameState {
  isInBatchTestStage :: Bool,
  batchTestStage :: [Note],
  singularTestStage :: [Note],
  foundNotes :: [Note],
  remainingOctaves :: [Octave],
  foundOctaves :: [Octave],
  octavePermutations :: [[Octave]]
} deriving (Show)

data Note = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord)
data Octave = O1 | O2 | O3
  deriving (Show,Eq,Ord)




initialGuess =
    ([Pitch A O1,Pitch B O1,Pitch A O1],GameState True [A,B,C,D,E,F] [G] [] [O1,O2,O3] [] [])

nextGuess (x:xs, beforeGameState) (correctPitches,correctNotes,correctOcts) =
   getGuessForState (updateState (x:xs,beforeGameState)(correctPitches,correctNotes,correctOcts))





-- updates the state of the game according to the feedback received
updateState (x:xs, gameState) (correctPitches,correctNotes,correctOcts)=

  if (length (foundNotes gameState) == 3 && length (foundOctaves gameState) == 3)
  then
      gameState
  else
      if isInBatchTestStage gameState
      then
        if (correctPitches==0 && correctNotes==0)
        then -- update state -> remove notes from batch array, if empty batch array change stage
          let newArr = filterNotes (batchTestStage gameState) (getNotes (x:xs)) in
            if length newArr == 0
            then (GameState False newArr (singularTestStage gameState) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) (octavePermutations gameState))
            else (GameState True newArr (singularTestStage gameState) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) (octavePermutations gameState))
        else -- update state -> remove notes from batch array and add to singular stage, if empty batch array change stage
          let newArr = filterNotes (batchTestStage gameState)(getNotes (x:xs)) in
            if length newArr == 0
            then (GameState False [] (nub((getNotes (x:xs))++(singularTestStage gameState))) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) (octavePermutations gameState))
            else (GameState True newArr (nub((getNotes (x:xs))++(singularTestStage gameState))) (foundNotes gameState) (remove (getOctaves(x:xs)!!0)
                            (remainingOctaves gameState)) (addToListWithCap (getOctaves(x:xs)!!0) (foundOctaves gameState) (correctPitches+correctOcts) 3) (octavePermutations gameState))
      else
        if (correctPitches ==0 && correctNotes ==0 )
        then GameState False [] (filterNotes (singularTestStage gameState) (getNotes (x:xs))) (foundNotes gameState) (remainingOctaves gameState) (foundOctaves gameState) (octavePermutations gameState)-- remove from singular array
        else
          let newSing = (addElementThisTimes (getNotes(x:xs)!!0) (foundNotes gameState) (correctNotes+correctPitches)) in
          if length newSing == 3
          then
            GameState False [] (filterNotes (singularTestStage gameState) (getNotes (x:xs))) newSing (remainingOctaves gameState) (foundOctaves gameState) (permutations (foundOctaves gameState))-- remove from singular array add to found notes
          else
            GameState False [] (filterNotes (singularTestStage gameState) (getNotes (x:xs))) newSing (remainingOctaves gameState) (foundOctaves gameState) (octavePermutations gameState)-- remove from singular array add to found notes


-- given a gameState returns the best next guess
getGuessForState gameState =
  if ((length (foundNotes gameState) == 3)&&(length (foundOctaves gameState) == 3))
  then
    let x:xs = (octavePermutations gameState) in
      ([Pitch ((foundNotes gameState)!!0) (x!!0),Pitch ((foundNotes gameState)!!1) (x!!1), Pitch ((foundNotes gameState)!!2) (x!!2)],
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


-- adds element to the list a number of times - if resulting list is at 'cap' length it stops adding
addToListWithCap element list 0 cap =
  list
addToListWithCap element list count cap=
  if length list >= cap
  then list
  else addToListWithCap element (element:list) (count-1) cap



-- adds the element a number of times to the target list
addElementThisTimes element list 0 =
  list
addElementThisTimes element list count=
  addElementThisTimes element (element:list) (count-1)



-- toPitch
toPitch pitchString
    | (length pitchString /= 2) = Nothing
    | elem (pitchString!!0) ['A','B','C','D','E','F','G'] == False = Nothing
    | elem (pitchString!!1) ['1','2','3'] == False = Nothing
    | otherwise = Just (Pitch (getNoteFromChar (pitchString !! 0)) (getOctaveFromChar (pitchString !! 1)))

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
-- toPitch end





-- feedback
feedback (x:xs) (x1:xs1) =
  feedback_helper (x:xs) (x1:xs1) (0,0,0)


-- first sort according to both notes and octaves and find number of equal pitches
-- after that use the intersection function to find number of common notes and octaves from the remaining array
feedback_helper [] [] x=
  x
feedback_helper (x:xs) (x1:xs1) (pitch,note,octave)=
   let (l1,l2,equal_pitches) = pitches (sortBy sortPitch(x:xs)) (sortBy sortPitch(x1:xs1)) 0 in
     (equal_pitches,notes l1 l2,octaves l1 l2)

pitches [] [] equal_pitches =
  ([],[],equal_pitches)

-- gives number of equal pitches in two array of pitches, and only returns remaining array that pitches werent equal
pitches (x:xs) (x1:xs1) equal_pitches =
  if x == x1
  then pitches xs xs1 (equal_pitches+1)
  else (x:xs,x1:xs1,equal_pitches)

notes l1 l2  =
    inters (getNotes l1) (getNotes l2) 0

octaves l1 l2  =
    inters (getOctaves l1) (getOctaves l2) 0

inters target [] count=
  count
-- intersection of two arrays with a twist: an element should be repeated for example twice in both arrays to appear
-- twice in the resulting array. e.g. inters [A,C,C] [C,D] = [C]
inters target (x1:xs1) count=
  if (elem x1 target)
  then inters (delete x1 target) xs1 (count+1)
  else inters target xs1 count
-- end feedback




-- function to compare two pitches
sortPitch (Pitch note1 oct1) (Pitch note2 oct2)
  | note1 < note2 = LT
  | note1 > note2 = GT
  | note1 == note2 = compare oct1 oct2



-- gets array of notes for the given note array
getNotes [] =
  []
getNotes ((Pitch note octave):xs) =
  note:(getNotes xs)



--gets array of octaves for the given pitch array
getOctaves [] =
  []
getOctaves ((Pitch note octave):xs) =
  octave:(getOctaves xs)


-- filters notes in first array by using the notes in the second array
filterNotes array notes =
  filter (\note -> not(elem note notes)) array



