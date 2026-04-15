module Main where

import System.Environment (getArgs)
import Scrambler
import Display
import Move
import Cube
import Solve.Cross 
import Solve.Solver

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
showCubeFlags "-e"  = print solvedEdges
showCubeFlags "-c"  = print solvedCorners
showCubeFlags "-u"  = print (upFace solvedCube)
showCubeFlags "-d"  = print (downFace solvedCube)
showCubeFlags "-f"  = print (frontFace solvedCube)
showCubeFlags "-b"  = print (backFace solvedCube)
showCubeFlags "-r"  = print (rightFace solvedCube)
showCubeFlags "-l"  = print (leftFace solvedCube)
showCubeFlags "-U"  = print (applyMoveList solvedCube [U])
showCubeFlags "-Up" = print (applyMoveList solvedCube [Up])
showCubeFlags "-D"  = print (applyMoveList solvedCube [D])
showCubeFlags "-Dp" = print (applyMoveList solvedCube [Dp])
showCubeFlags "-F"  = print (applyMoveList solvedCube [F])
showCubeFlags "-Fp" = print (applyMoveList solvedCube [Fp])
showCubeFlags "-B"  = print (applyMoveList solvedCube [B])
showCubeFlags "-Bp" = print (applyMoveList solvedCube [Bp])
showCubeFlags "-R"  = print (applyMoveList solvedCube [R])
showCubeFlags "-Rp" = print (applyMoveList solvedCube [Rp])
showCubeFlags "-L"  = print (applyMoveList solvedCube [L])
showCubeFlags "-Lp" = print (applyMoveList solvedCube [Lp])
showCubeFlags "--solve" = solve
showCubeFlags x     = putStrLn ("Could not find flag " ++ x)


solve :: IO ()
solve = do
  scrambleMoves <- scramble 10
  print scrambleMoves
  let cube = applyMoveList solvedCube scrambleMoves
  print cube
  case search cube 8 whiteCrossSolved of
    Nothing -> print "Cross: no solution found"
    Just (crossCube, crossSolution) -> do
      print crossSolution
      print crossCube
