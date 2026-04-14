module MoveSpec where
import Model

data MoveSpec = MoveSpec
    { cornerCycle  :: [CornerPosition]
    , cornerDeltas :: [[Int]]
    , edgeCycle    :: [EdgePosition]
    , edgeDeltas   :: [Int]
    }

rMove :: MoveSpec
rMove = MoveSpec
    { cornerCycle  = [DRF, URF, URB, DRB]
    , cornerDeltas = [[2,1,0], [2,1,0], [2,1,0], [2,1,0]]
    , edgeCycle    = [RF, UR, RB, DR]
    , edgeDeltas   = [1, 1, 1, 1]
    }

lMove :: MoveSpec
lMove = MoveSpec
    { cornerCycle  = reverse [DLF, ULF, ULB, DLB]
    , cornerDeltas = [[2,1,0], [2,1,0], [2,1,0], [2,1,0]]
    , edgeCycle    = reverse [LF, UL, LB, DL]
    , edgeDeltas   = [1, 1, 1, 1]
    }

uMove :: MoveSpec
uMove = MoveSpec
    { cornerCycle  = [URF, ULF, ULB, URB]
    , cornerDeltas = [[0,2,1], [0,2,1], [0,2,1], [0,2,1]]
    , edgeCycle    = [UF, UL, UB, UR]
    , edgeDeltas   = [0, 0, 0, 0]
    }

dMove :: MoveSpec
dMove = MoveSpec
    { cornerCycle  = reverse [DRF, DLF, DLB, DRB]
    , cornerDeltas = [[0,2,1], [0,2,1], [0,2,1], [0,2,1]]
    , edgeCycle    = reverse [DF, DL, DB, DR]
    , edgeDeltas   = [0, 0, 0, 0]
    }


fMove :: MoveSpec
fMove = MoveSpec
    { cornerCycle  = [ULF, URF, DRF, DLF]
    , cornerDeltas = [[1,0,2], [1,0,2], [1,0,2], [1,0,2]]
    , edgeCycle    = [UF, RF, DF, LF]
    , edgeDeltas   = [0, 0, 0, 0]
    }

bMove :: MoveSpec
bMove = MoveSpec
    { cornerCycle  = reverse [ULB, URB, DRB, DLB]
    , cornerDeltas = [[1,0,2], [1,0,2], [1,0,2], [1,0,2]]
    , edgeCycle    = reverse [UB, RB, DB, LB]
    , edgeDeltas   = [0, 0, 0, 0]
    }