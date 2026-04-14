module Main where

import System.Environment (getArgs)
import Scrambler 
import Display
import Move
import Cube

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scramble":numberOfMoves:_) -> do 
      let moves = read numberOfMoves :: Int
      runScramble moves
    ("scramble":_) -> runScramble 25
    ("show":x:_) -> showCubeFlags x 
    ("show":_) -> print solvedCube                                                   
    _ -> putStrLn "Cound not find command"


runScramble ::Int -> IO ()
runScramble num = do
  moves <- scramble num
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
