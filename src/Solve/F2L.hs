module Solve.F2L where

import Solve.Cross (whiteCrossSolved)
import Model
import Move

f2LSolved :: Cube -> Bool
f2LSolved (Cube corners edges) = whiteCrossSolved (Cube corners edges) &&
    (rf edges == EdgeCubie RF ENeutral) &&
    (lf edges == EdgeCubie LF ENeutral) &&
    (lb edges == EdgeCubie LB ENeutral) &&
    (rb edges == EdgeCubie RB ENeutral) 

    -- (urf corners == CornerCubie URF (CornerOri 0 1 2)) &&
    -- (ulf corners == CornerCubie ULF (CornerOri 0 1 2)) &&
    -- (urb corners == CornerCubie URB (CornerOri 0 1 2)) &&
    -- (ulb corners == CornerCubie ULB (CornerOri 0 1 2))
