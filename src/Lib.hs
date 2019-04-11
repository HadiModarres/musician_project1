module Lib
    ( someFunc,
    Pitch,
    toPitch,
    feedback,
    test,
    GameState
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
  currentStage :: Int,
  stage1 :: [Note],
  stage2 :: [Note],
  foundNotes :: [Note]
}



--initialGuess =
--  ()

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

test =
--  feedback [(Pitch A One)] [(Pitch B Two)]
--   feedback ( [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]) ([(Pitch A O1),(Pitch D O1),(Pitch C O5),(Pitch A O3)])
--    foldl1 Data.List.intersect [[C,D,C], [D,C,C]]
--    getNotes [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]
    toPitch "R1"

someFunc :: IO ()
someFunc = putStrLn "someFunc"




