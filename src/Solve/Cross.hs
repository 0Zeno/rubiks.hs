module Solve.Cross where
import Model
import Move
import Debug.Trace

whiteCrossSolved :: Cube -> Bool
whiteCrossSolved (Cube _ edges) =
    (uf edges == EdgeCubie UF Neutral) &&
    (ul edges == EdgeCubie UL Neutral) &&
    (ur edges == EdgeCubie UR Neutral) &&
    (ub edges == EdgeCubie UB Neutral)

solveWhiteCross :: Cube -> Cube 
solveWhiteCross = solveUF . solveUB . solveUL . solveUR

allEdgeSlots :: [EdgePosition]
allEdgeSlots = [UF, UR, UL, UB, DF, DR, DL, DB, RF, LF, RB, LB]

cubiePos :: EdgeCubie -> EdgePosition
cubiePos (EdgeCubie pos _) = pos

findEdgeCubie :: Edges -> EdgePosition -> EdgePosition
findEdgeCubie edges target = head (filter (\s -> cubiePos (getEdge edges s) == target) allEdgeSlots)

insertFromD :: EdgeCubie -> EdgePosition -> [Move]
insertFromD (EdgeCubie _ Neutral) DF = [F2]
insertFromD (EdgeCubie _ Flipped) DF = [F, U, Lp, Up]
insertFromD (EdgeCubie _ Neutral) DR = [R2]
insertFromD (EdgeCubie _ Flipped) DR = [R, U, Fp, Up]
insertFromD (EdgeCubie _ Neutral) DL = [L2]
insertFromD (EdgeCubie _ Flipped) DL = [Lp, Up, F, U]
insertFromD (EdgeCubie _ Neutral) DB = [B2]
insertFromD (EdgeCubie _ Flipped) DB = [B, U, Rp, Up]
insertFromD _ _ = error "insertFromD failed in white cross"

solveUF :: Cube -> Cube
solveUF cube@(Cube _ edges) =
    let pos = findEdgeCubie edges UF
    in case pos of
        UF -> if getEdge edges UF == EdgeCubie UF Neutral
            then cube
            else solveUF (applyMoveList cube [F2])
        DF -> applyMoveList cube (insertFromD (getEdge edges pos ) DF)
        DR -> solveUF (applyMoveList cube [Dp])
        DL -> solveUF (applyMoveList cube [D])
        DB -> solveUF (applyMoveList cube [D2])
        UR -> solveUF (applyMoveList cube [R2])
        UL -> solveUF (applyMoveList cube [L2])
        UB -> solveUF (applyMoveList cube [B2])
        RF -> solveUF (applyMoveList cube [Rp, D, R])
        LF -> solveUF (applyMoveList cube [Fp, D, F])
        RB -> solveUF (applyMoveList cube [Bp, D, B])
        LB -> solveUF (applyMoveList cube [Lp, D, L])
        
        
solveUR :: Cube -> Cube
solveUR cube@(Cube _ edges) =
    let pos = findEdgeCubie edges UR
    in case pos of
        UR -> if getEdge edges UR == EdgeCubie UR Neutral
            then cube
            else solveUR (applyMoveList cube [R2])
        DR -> applyMoveList cube (insertFromD (getEdge edges pos) DR)
        DF -> solveUR (applyMoveList cube [D])
        DL -> solveUR (applyMoveList cube [D2])
        DB -> solveUR (applyMoveList cube [Dp])
        UF -> solveUR (applyMoveList cube [F2])
        UL -> solveUR (applyMoveList cube [L2])
        UB -> solveUR (applyMoveList cube [B2])
        RF -> solveUR (applyMoveList cube [Rp, D, R])
        LF -> solveUR (applyMoveList cube [Fp, D, F])
        RB -> solveUR (applyMoveList cube [Bp, D, B])
        LB -> solveUR (applyMoveList cube [Lp, D, L])

solveUL :: Cube -> Cube
solveUL cube@(Cube _ edges) =
    let pos = findEdgeCubie edges UL
    in case pos of
        UL -> if getEdge edges UL == EdgeCubie UL Neutral
            then cube
            else solveUL (applyMoveList cube [L2])
        DL -> applyMoveList cube (insertFromD (getEdge edges pos) DL)
        DF -> solveUL (applyMoveList cube [Dp])
        DR -> solveUL (applyMoveList cube [D2])
        DB -> solveUL (applyMoveList cube [D])
        UF -> solveUL (applyMoveList cube [F2])
        UR -> solveUL (applyMoveList cube [R2])
        UB -> solveUL (applyMoveList cube [B2])
        RF -> solveUL (applyMoveList cube [Rp, D, R])
        LF -> solveUL (applyMoveList cube [Fp, D, F])
        RB -> solveUL (applyMoveList cube [Bp, D, B])
        LB -> solveUL (applyMoveList cube [Lp, D, L])

solveUB :: Cube -> Cube
solveUB cube@(Cube _ edges) =
    let pos = findEdgeCubie edges UB
    in case pos of
        UB -> if getEdge edges UB == EdgeCubie UB Neutral
            then cube
            else solveUB (applyMoveList cube [B2])
        DB -> applyMoveList cube (insertFromD (getEdge edges pos) DB)
        DF -> solveUB (applyMoveList cube [D2])
        DR -> solveUB (applyMoveList cube [D])
        DL -> solveUB (applyMoveList cube [Dp])
        UF -> solveUB (applyMoveList cube [F2])
        UR -> solveUB (applyMoveList cube [R2])
        UL -> solveUB (applyMoveList cube [L2])
        RF -> solveUB (applyMoveList cube [Rp, D, R])
        LF -> solveUB (applyMoveList cube [Fp, D, F])
        RB -> solveUB (applyMoveList cube [Bp, D, B])
        LB -> solveUB (applyMoveList cube [Lp, D, L])