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
    ("show":x:_) -> showCubeFlags x 
    ("show":_) -> print solvedCube
    _ -> putStrLn "Cound not find showCubeFlags"


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
