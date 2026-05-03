module Helper where
import Move
import Model

inverse :: Move -> Move
inverse U = D
inverse D = U
inverse L = R
inverse R = L
inverse B = F
inverse F = B
inverse Up = Dp
inverse Dp = Up
inverse Lp = Rp
inverse Rp = Lp
inverse Bp = Fp
inverse Fp = Bp
inverse U2 = U2
inverse D2 = D2
inverse L2 = L2
inverse R2 = R2
inverse B2 = B2
inverse F2 = F2

cubiePos :: CornerCubie -> CornerPosition
cubiePos (CornerCubie pos _) = pos