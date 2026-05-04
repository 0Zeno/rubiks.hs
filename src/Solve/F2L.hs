module Solve.F2L where

import Solve.Cross (whiteCrossSolved)
import Model
import Move

f2LSolved :: Cube -> Bool
f2LSolved (Cube corners edges) = whiteCrossSolved (Cube corners edges) &&
    (rf edges == EdgeCubie RF Neutral) &&
    (lf edges == EdgeCubie LF Neutral) &&
    (lb edges == EdgeCubie LB Neutral) &&
    (rb edges == EdgeCubie RB   Neutral) &&
    (urf corners == CornerCubie URF (CornerOri 0 1 2)) &&
    (ulf corners == CornerCubie ULF (CornerOri 0 1 2)) &&
    (urb corners == CornerCubie URB (CornerOri 0 1 2)) &&
    (ulb corners == CornerCubie ULB (CornerOri 0 1 2))

whiteCornersSolved :: Cube -> Bool
whiteCornersSolved (Cube corners _) =
    (urf corners == CornerCubie URF (CornerOri 0 1 2)) &&
    (ulf corners == CornerCubie ULF (CornerOri 0 1 2)) &&
    (urb corners == CornerCubie URB (CornerOri 0 1 2)) &&
    (ulb corners == CornerCubie ULB (CornerOri 0 1 2))

solveWhiteSide :: Cube -> Cube 
solveWhiteSide = solveULF . solveULB . solveURB . solveURF

allCornerSlots :: [CornerPosition]
allCornerSlots = [URF, ULF, URB, ULB, DRF, DLF, DRB, DLB]

cubiePos :: CornerCubie -> CornerPosition
cubiePos (CornerCubie pos _) = pos

findCornerCubie :: Corners -> CornerPosition -> CornerPosition
findCornerCubie corners target = head (filter (\s -> cubiePos (getCorner corners s) == target) allCornerSlots)


replicateMoves :: Int -> [Move] -> [Move]
replicateMoves n list = concat (replicate n list)

insertFromD :: CornerCubie -> CornerPosition -> [Move]
insertFromD (CornerCubie _ (CornerOri _ _ 0)) DRF = [F, D, Fp]
insertFromD (CornerCubie _ (CornerOri _ 0 _)) DRF = [Rp, Dp, R]
insertFromD (CornerCubie _ (CornerOri 0 _ _)) DRF = [Rp, D2, R, D, Rp, Dp, R]
insertFromD (CornerCubie _ (CornerOri _ _ 0)) DLF = [Fp, Dp, F]
insertFromD (CornerCubie _ (CornerOri _ 0 _)) DLF = [L, D, Lp]
insertFromD (CornerCubie _ (CornerOri 0 _ _)) DLF = [L, D2, Lp, Dp, L, D, Lp]
insertFromD (CornerCubie _ (CornerOri _ _ 0)) DRB = [Bp, Dp, B]
insertFromD (CornerCubie _ (CornerOri _ 0 _)) DRB = [R, D, Rp]
insertFromD (CornerCubie _ (CornerOri 0 _ _)) DRB = [Bp, D2, B, D, Bp, Dp, B]
insertFromD (CornerCubie _ (CornerOri _ _ 0)) DLB = [B, D, Bp]
insertFromD (CornerCubie _ (CornerOri _ 0 _)) DLB = [Lp, Dp, L]
insertFromD (CornerCubie _ (CornerOri 0 _ _)) DLB = [B, D2, Bp, Dp, B, D, Bp]

solveURF :: Cube -> Cube
solveURF cube@(Cube corners _) =
  let pos = findCornerCubie corners URF
  in case pos of 
    URF -> if getCorner corners URF == CornerCubie URF (CornerOri 0 1 2)
           then cube 
           else solveURF (applyMoveList cube [Rp, Dp, R, D])
    DRF -> applyMoveList cube (insertFromD (getCorner corners pos) DRF)   
    DLF -> solveURF (applyMoveList cube [D])
    DLB -> solveURF (applyMoveList cube [D2])
    DRB -> solveURF (applyMoveList cube [Dp])    
    ULF -> solveURF (applyMoveList cube [L, D, Lp])  
    ULB -> solveURF (applyMoveList cube [B, D2, Bp])
    URB -> solveURF (applyMoveList cube [Bp, Dp, B])

solveULF :: Cube -> Cube
solveULF cube@(Cube corners _) =
  let pos = findCornerCubie corners ULF
  in case pos of 
    ULF -> if getCorner corners ULF == CornerCubie ULF (CornerOri 0 1 2)
           then cube 
           else solveULF (applyMoveList cube [L, Dp, Lp, D])
    DLF -> applyMoveList cube (insertFromD (getCorner corners pos) DLF)   
    DRF -> solveULF (applyMoveList cube [Dp])
    DLB -> solveULF (applyMoveList cube [D])
    DRB -> solveULF (applyMoveList cube [D2])    
    URF -> solveULF (applyMoveList cube [Rp, D, R])  
    ULB -> solveULF (applyMoveList cube [B, D, Bp])
    URB -> solveULF (applyMoveList cube [Bp, D2, B])

solveURB :: Cube -> Cube
solveURB cube@(Cube corners _) =
  let pos = findCornerCubie corners URB
  in case pos of 
    URB -> if getCorner corners URB == CornerCubie URB (CornerOri 0 1 2)
           then cube 
           else solveURB (applyMoveList cube [R, Dp, Rp])
    DRB -> applyMoveList cube (insertFromD (getCorner corners pos) DRB)   
    DLF -> solveURB (applyMoveList cube [D2])
    DLB -> solveURB (applyMoveList cube [Dp])
    DRF -> solveURB (applyMoveList cube [D])    
    ULF -> solveURB (applyMoveList cube [L, D2, Lp])  
    ULB -> solveURB (applyMoveList cube [B, D2, Bp, D])
    URF -> solveURB (applyMoveList cube [F, D, Fp])

solveULB :: Cube -> Cube
solveULB cube@(Cube corners _) =
  let pos = findCornerCubie corners ULB
  in case pos of 
    ULB -> if getCorner corners ULB == CornerCubie ULB (CornerOri 0 1 2)
           then cube 
           else solveULB (applyMoveList cube [Lp, Dp, L, D])
    DRB -> solveULB (applyMoveList cube [D])
    DLF -> solveULB (applyMoveList cube [Dp])
    DLB -> applyMoveList cube (insertFromD (getCorner corners pos) DLB)
    DRF -> solveULB (applyMoveList cube [D2])    
    ULF -> solveULB (applyMoveList cube [L, D2, Lp, D])  
    URB -> solveULB (applyMoveList cube [R, D, Rp])
    URF -> solveULB (applyMoveList cube [Rp, D2, R])