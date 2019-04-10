module Lib
    ( someFunc,
    Pitch,
--    toPitch,
    feedback,
    test
--    GameState,
--    initialGuess,
--    nextGuess
    ) where

import Data.List
--toPitch :: String -> Maybe Pitch
feedback ::[Pitch] -> [Pitch] -> (Int,Int,Int)
--initialGuess :: ([Pitch],GameState)
--nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)

data Note = A | B | C | D
  deriving (Show,Eq,Ord)
data Octave = O1 | O2 | O3 | O4 | O5
  deriving (Show,Eq,Ord)

data Pitch = Pitch Note Octave
  deriving (Show,Eq)



feedback (x:xs) (x1:xs1) =
  feedback_helper (x:xs) (x1:xs1) (0,0,0)

feedback_helper [] [] x=
  x
feedback_helper (x:xs) (x1:xs1) (pitch,note,octave)=
  (0,0,0)

pitches [] [] equal_pitches =
  ([],[],equal_pitches)

pitches (x:xs) (x1:xs1) equal_pitches =
  if x == x1
  then pitches xs xs1 (equal_pitches+1)
  else (x:xs,x1:xs1,equal_pitches)

sortPitch (Pitch note1 oct1) (Pitch note2 oct2)
  | note1 < note2 = LT
  | note1 > note2 = GT
  | note1 == note2 = compare oct1 oct2


test =
--  feedback [(Pitch A One)] [(Pitch B Two)]
   pitches (sortBy sortPitch [(Pitch C O2),(Pitch B O1),(Pitch A O3),(Pitch A O1)]) (sortBy sortPitch [(Pitch A O1),(Pitch D O1),(Pitch C O5),(Pitch A O3)]) 0



someFunc :: IO ()
someFunc = putStrLn "someFunc"




