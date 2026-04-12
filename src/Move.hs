{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Move where
import Model (CornerPosition(..), EdgePosition(..), Corners(..), CornerCubie (..), Edges(..), EdgeCubie (..), Cube(..), CornerOrientaion (..), EdgeOrientation(..))

data Move = U | D | F | B | L | R
    deriving (Show, Eq)

data MoveSpec = MoveSpec
    { cornerCycle  :: [CornerPosition]
    , cornerDeltas :: [[Int]]
    , edgeCycle    :: [EdgePosition]
    , edgeDeltas   :: [Int]
    }

applyMoveSpec :: Cube -> MoveSpec -> Cube
applyMoveSpec cube move =
    let (Cube corners edges) = cube
        newCorners = applyCornerCycle corners (cornerCycle move) (cornerDeltas move)
        newEdges = applyEdgeCycle edges (edgeCycle move) (edgeDeltas move)
    in Cube newCorners newEdges

rMove :: MoveSpec
rMove = MoveSpec
    { cornerCycle  = [DRF, URF, URB, DRB]
    , cornerDeltas = [[2,1,0], [2,1,0], [2,1,0], [2,1,0]]
    , edgeCycle    = [RF, UR, RB, DR]
    , edgeDeltas   = [1, 1, 1, 1]
    }

getCorner :: Corners -> CornerPosition -> CornerCubie
getCorner corners URF = urf corners
getCorner corners URB = urb corners
getCorner corners DRF = drf corners
getCorner corners DRB = drb corners
getCorner corners ULF = ulf corners
getCorner corners ULB = ulb corners
getCorner corners DLF = dlf corners
getCorner corners DLB = dlb corners

setCorner :: Corners -> CornerPosition -> CornerCubie -> Corners
setCorner corners URF cubie = corners {urf = cubie}
setCorner corners URB cubie = corners {urb = cubie}
setCorner corners DRF cubie = corners {drf = cubie}
setCorner corners DRB cubie = corners {drb = cubie}
setCorner corners ULF cubie = corners {ulf = cubie}
setCorner corners ULB cubie = corners {ulb = cubie}
setCorner corners DLF cubie = corners {dlf = cubie}
setCorner corners DLB cubie = corners {dlb = cubie}

twistCorner :: CornerCubie -> [Int] -> CornerCubie
twistCorner (CornerCubie pos (CornerOri a b c)) delta =
    let ori          = [a, b, c]
        [a', b', c'] = map (ori !!) delta
    in CornerCubie pos (CornerOri a' b' c')

applyCornerCycle :: Corners -> [CornerPosition] -> [[Int]] -> Corners
applyCornerCycle corners positions deltas =
    let fromTo = zip3 positions (tail positions ++ [head positions]) deltas
        cubies = map (\(from, _, _) -> getCorner corners from) fromTo
    in foldl (\cs ((_, to, delta), cubie) -> setCorner cs to (twistCorner cubie delta)) corners (zip fromTo cubies)

getEdge :: Edges -> EdgePosition -> EdgeCubie
getEdge edges UF = uf edges
getEdge edges UB = ub edges
getEdge edges UR = ur edges
getEdge edges UL = ul edges
getEdge edges DF = df edges
getEdge edges DB = db edges
getEdge edges DR = dr edges
getEdge edges DL = dl edges
getEdge edges RF = rf edges
getEdge edges LF = lf edges
getEdge edges RB = rb edges
getEdge edges LB = lb edges

setEdge :: Edges -> EdgePosition -> EdgeCubie -> Edges
setEdge edges UF cubie = edges {uf = cubie}
setEdge edges UB cubie = edges {ub = cubie}
setEdge edges UR cubie = edges {ur = cubie}
setEdge edges UL cubie = edges {ul = cubie}
setEdge edges DF cubie = edges {df = cubie}
setEdge edges DB cubie = edges {db = cubie}
setEdge edges DR cubie = edges {dr = cubie}
setEdge edges DL cubie = edges {dl = cubie}
setEdge edges RF cubie = edges {rf = cubie}
setEdge edges LF cubie = edges {lf = cubie}
setEdge edges RB cubie = edges {rb = cubie}
setEdge edges LB cubie = edges {lb = cubie}

twistEdge :: EdgeCubie -> Int -> EdgeCubie
twistEdge (EdgeCubie pos ori) n =
        EdgeCubie pos (edgeIntToOri (edgeOriToInt ori + n))

edgeOriToInt :: EdgeOrientation -> Int
edgeOriToInt ori =
    case ori of
        ENeutral -> 0
        EFlipped -> 1

edgeIntToOri :: Int -> EdgeOrientation
edgeIntToOri num =
    let oriNum = mod num 2
    in case oriNum of
        0 -> ENeutral 
        1 -> EFlipped

applyEdgeCycle:: Edges -> [EdgePosition] -> [Int] -> Edges
applyEdgeCycle edges positions deltas =
    let fromTo = zip3 positions (tail positions ++ [head positions]) deltas
        cubies = map (\(from, _, _) -> getEdge edges from) fromTo
    in foldl (\cs ((_, to, delta), cubie) -> setEdge cs to (twistEdge cubie delta)) edges (zip fromTo cubies)