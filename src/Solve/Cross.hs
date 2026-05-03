module Solve.Cross where
import Model
import Move

whiteCrossSolved :: Cube -> Bool
whiteCrossSolved (Cube _ edges) =
    (uf edges == EdgeCubie UF Neutral) &&
    (ul edges == EdgeCubie UL Neutral) &&
    (ur edges == EdgeCubie UR Neutral) &&
    (ub edges == EdgeCubie UB Neutral)

allEdgeSlots :: [EdgePosition]
allEdgeSlots = [UF, UR, UL, UB, DF, DR, DL, DB, RF, LF, RB, LB]

cubiePos :: EdgeCubie -> EdgePosition
cubiePos (EdgeCubie pos _) = pos

findEdgeCubie :: Edges -> EdgePosition -> EdgePosition
findEdgeCubie edges target = head (filter (\s -> cubiePos (getEdge edges s) == target) allEdgeSlots)
