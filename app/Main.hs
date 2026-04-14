module Main where

import System.Environment (getArgs)
import Scrambler 
import Model
import Display
import Move
import MoveSpec

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scramble":numberOfMoves:_) -> do 
      moves <- scramble (read numberOfMoves :: Int)
      print moves
    ("scramble":_) -> runScramble
    ("show":x:_) -> showCubeFlags x 
    ("show":_) -> print (applyMoveSpec solvedCube bMove)                                                   
    _ -> putStrLn "Cound not find command"


runScramble :: IO ()
runScramble = do
  moves <- scramble 25
  print moves
  print (applyMoveList solvedCube moves)

showCubeFlags :: String -> IO ()
showCubeFlags "-e" = print solvedEdges
showCubeFlags "-c" = print solvedCorners
showCubeFlags "-u" = print (upFace solvedCube)
showCubeFlags "-d" = print (downFace solvedCube)
showCubeFlags "-f" = print (frontFace solvedCube)
showCubeFlags "-b" = print (backFace solvedCube)
showCubeFlags "-r" = print (rightFace solvedCube)
showCubeFlags "-l" = print (leftFace solvedCube)
showCubeFlags x = putStrLn ("Could not find flag " ++ x) 
