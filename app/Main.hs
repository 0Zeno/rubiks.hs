module Main where
import System.Environment (getArgs)
import Scrambler
import Display
import Move
import Cube
import Solve.Cross 
import Solve.F2L
import Solve.Solver
import Model

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
    ("solve":_) -> solve
    ("manual":_) -> manualSolve
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
showCubeFlags x     = putStrLn ("Could not find flag " ++ x)


solve :: IO ()
solve = do
  scrambleMoves <- scramble 10
  putStrLn "Scramble:"
  print scrambleMoves
  let cube = applyMoveList solvedCube scrambleMoves
  putStrLn "Scrambled cube:"
  print cube
  let cube2 = solveWhiteSide cube
  putStrLn "After solving white corners:"
  print cube2
  putStrLn $ if whiteCornersSolved cube2 then "White corners solved!" else "White corners NOT solved."
  



manualSolve :: IO ()
manualSolve = do
  putStrLn "Hello and welcome to the manual solver"          
  scrambleMoves <- scramble 25
  print scrambleMoves
  let cube = applyMoveList solvedCube scrambleMoves
  loop cube

loop :: Cube -> IO ()
loop cube = do
  print cube
  if cube == solvedCube 
    then print "Solved!!"
  else do
    line <- getLine 
    let result = reads line :: [(Move, String)]
    case result of
      [(move, _)] -> loop (applyMoveList cube [move])
      _ -> do putStrLn "Invalid move, Try agains"
              loop cube

