module Main where

import System.Environment (getArgs)
import qualified Scrambler (scramble)
import Model 
import Display

solvedCube :: Cube 
solvedCube = Cube {
  left = orangeFace,
  up = yellowFace,
  front = greenFace,
  down = whiteFace,
  right = redFace,
  back = blueFace
} 

greenFace :: Face
greenFace = Face (Row Green Green Green)
                 (Row Green Green Green)
                 (Row Green Green Green)

yellowFace :: Face
yellowFace = Face (Row Yellow Yellow Yellow)
                  (Row Yellow Yellow Yellow)
                  (Row Yellow Yellow Yellow)

redFace :: Face
redFace = Face (Row Red Red Red)
               (Row Red Red Red)
               (Row Red Red Red)

whiteFace :: Face
whiteFace = Face (Row White White White)
                 (Row White White White)
                 (Row White White White)

blueFace:: Face
blueFace = Face (Row Blue Blue Blue)
                (Row Blue Blue Blue)
                (Row Blue Blue Blue)

orangeFace :: Face
orangeFace = Face (Row Orange Orange Orange)
                  (Row Orange Orange Orange)
                  (Row Orange Orange Orange)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ("scramble":numberOfMoves:_) -> Scrambler.scramble ( read numberOfMoves :: Int )
    ("scramble":_) -> Scrambler.scramble 25
    ("show":_) -> putStrLn (show solvedCube)
    _ -> putStrLn "Cound not find command"
