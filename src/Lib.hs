module Lib
    ( someFunc,
    Pitch,
    toPitch,
    feedback,
    GameState,
    initialGuess,
    nextGuess
    ) where

toPitch :: String -> Maybe Pitch
feedback ::[Pitch] -> [Pitch] -> (Int,Int,Int)
initialGuess :: ([Pitch],GameState)
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)

data Note = A | B
  deriving Show
data Octave = One | Two
  deriving Show

data Pitch = Pitch Note Octave



someFunc :: IO ()
someFunc = putStrLn "someFunc"




