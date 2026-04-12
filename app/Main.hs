module Main where

import System.Environment (getArgs)
import qualified Scrambler (scramble)
import Model
import Display

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scramble":numberOfMoves:_) -> Scrambler.scramble ( read numberOfMoves :: Int )
    ("scramble":_) -> Scrambler.scramble 25
    ("show":x:_) -> command x 
    ("show":_) -> print solvedCube
    _ -> putStrLn "Cound not find command"


command :: String -> IO ()
command "-e" = print solvedEdges
command "-c" = print solvedCorners
command "-u" = print (upFace solvedCube)
command "-d" = print (downFace solvedCube)
command "-f" = print (frontFace solvedCube)
command "-b" = print (backFace solvedCube)
command "-r" = print (rightFace solvedCube)
command "-l" = print (leftFace solvedCube)
command x = putStrLn ("Could not find flag " ++ x) 
