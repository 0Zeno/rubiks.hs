module Solve.Solver where
import Model
import Move
import Control.Monad (msum)
import Data.List (find)


searchDepth :: Cube -> Int -> (Cube -> Bool) -> Maybe (Cube, [Move])
searchDepth cube n f =
    fmap (\moves -> (applyMoveList cube moves, moves))
         (find (\moves -> f (applyMoveList cube moves))
         (buildSequences (Nothing, Nothing) n))

search :: Cube -> Int -> (Cube -> Bool) -> Maybe (Cube, [Move])
search cube maxDepth f = msum [searchDepth cube d f | d <- [0..maxDepth]]

buildSequences :: (Maybe Move, Maybe Move) -> Int -> [[Move]]
buildSequences _ 0 = [[]]
buildSequences prev n = [ m : rest | m <- validNextMoves prev, rest <- buildSequences (snd prev, Just m) (n-1)]

validNextMoves :: (Maybe Move, Maybe Move) -> [Move]
validNextMoves (secondLast, lastMove) =
    filter valid allMoves
  where
    valid m = case lastMove of
        Nothing -> True
        Just l  -> m /= inverse l && not (secondLast == lastMove && Just m == lastMove)

inverse :: Move -> Move
inverse U = Up
inverse D = Dp
inverse F = Fp
inverse B = Bp
inverse L = Lp
inverse R = Rp
inverse Up = U
inverse Dp = D
inverse Fp = F
inverse Bp = B
inverse Lp = L
inverse Rp = R