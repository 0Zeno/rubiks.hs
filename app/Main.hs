module Main where
import System.Environment (getArgs)
import Scrambler
import Display()
import Move
import Cube
import Solve.F2L
import Model

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("scramble":numberOfMoves:_) -> do
      let moves = read numberOfMoves :: Int
      if moves < 0 
      then putStrLn "Negative amount of moves is not possible" 
      else runScramble moves
    ("scramble":_) -> runScramble 25
    ("show":_) -> print solvedCube
    ("solve":_) -> solve
    ("manual":_) -> manualSolve
    _ -> putStrLn "Cound not find command"


runScramble ::Int -> IO ()
runScramble num = do
  moves <- scramble num
  print moves
  print (applyMoveList solvedCube moves)

solve :: IO ()
solve = do
  scrambleMoves <- scramble 25
  putStrLn "Scramble:"
  print scrambleMoves
  let cube = applyMoveList solvedCube scrambleMoves
  putStrLn "Scrambled cube:"
  print cube
  let cube2 = solveWhiteSide cube
  putStrLn "After solving white corners:"
  print cube2

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

