module Solve.Cross where
import Model
import Move
import Control.Monad (msum, replicateM)
import Data.List (find)

whiteCrossSolved :: Cube -> Bool
whiteCrossSolved (Cube _ edges) =
    (uf edges == EdgeCubie UF ENeutral) &&
    (ul edges == EdgeCubie UL ENeutral) &&
    (ur edges == EdgeCubie UR ENeutral) &&
    (ub edges == EdgeCubie UB ENeutral)

