module Main where

import System.Environment (getArgs)
import qualified Scrambler (scramble)

main :: IO ()
main = do
  args <- getArgs
  case args of 
    ("scramble":numberOfMoves:_) -> Scrambler.scramble ( read numberOfMoves :: Int )
    ("scramble":_) -> Scrambler.scramble 25
    _ -> putStrLn "Cound not find command"
