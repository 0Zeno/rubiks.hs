module Model where

data Cube = 
    Cube Corners Edges

data Color =
    Red | Yellow | Green | Blue | White | Orange
    deriving (Eq)

data Corners = Corners
      { urf :: CornerCubie
      , ulf :: CornerCubie                                                      
      , urb :: CornerCubie
      , ulb :: CornerCubie                                                      
      , drf :: CornerCubie                                                      
      , dlf :: CornerCubie                                                     
      , drb :: CornerCubie                                                      
      , dlb :: CornerCubie
      }
      deriving (Show)          

data CornerCubie =
    CornerCubie CornerPosition CornerOrientaion
    deriving (Show)

data CornerPosition =
    URF | ULF | URB | ULB | DRF | DLF | DRB | DLB
    deriving (Show)

data CornerOrientaion =
    CornerOri Int Int Int
    deriving (Show)                                                               

data Edges = Edges                                                            
    { uf :: EdgeCubie
    , ur :: EdgeCubie                                                        
    , ul :: EdgeCubie
    , ub :: EdgeCubie
    , df :: EdgeCubie
    , dr :: EdgeCubie                                                         
    , dl :: EdgeCubie
    , db :: EdgeCubie                                                         
    , rf :: EdgeCubie
    , lf :: EdgeCubie                                                        
    , rb :: EdgeCubie
    , lb :: EdgeCubie                                                         
    }
    deriving (Show)                                                               
 
data EdgeCubie =
    EdgeCubie EdgePosition EdgeOrientation
    deriving (Show)

data EdgePosition = 
    UF | UR | UL | UB | DF | DR | DL | DB | RF | LF | RB | LB
    deriving (Show)                                                               

data EdgeOrientation = 
    ENeutral | EFlipped
    deriving (Show)                                                               


solvedCorners :: Corners
solvedCorners = Corners
    { urf = CornerCubie URF (CornerOri 0 1 2)
    , ulf = CornerCubie ULF (CornerOri 0 1 2)
    , urb = CornerCubie URB (CornerOri 0 1 2)
    , ulb = CornerCubie ULB (CornerOri 0 1 2)
    , drf = CornerCubie DRF (CornerOri 0 1 2)
    , dlf = CornerCubie DLF (CornerOri 0 1 2)
    , drb = CornerCubie DRB (CornerOri 0 1 2)
    , dlb = CornerCubie DLB (CornerOri 0 1 2)
    }

solvedEdges :: Edges
solvedEdges = Edges
    { uf = EdgeCubie UF ENeutral
    , ur = EdgeCubie UR ENeutral
    , ul = EdgeCubie UL ENeutral
    , ub = EdgeCubie UB ENeutral
    , df = EdgeCubie DF ENeutral
    , dr = EdgeCubie DR ENeutral
    , dl = EdgeCubie DL ENeutral
    , db = EdgeCubie DB ENeutral
    , rf = EdgeCubie RF ENeutral
    , lf = EdgeCubie LF ENeutral
    , rb = EdgeCubie RB ENeutral
    , lb = EdgeCubie LB ENeutral
    }

-- (upOrDown color, rightOrLeft color, frontOrBack color)      
cornerToColors :: CornerPosition -> (Color, Color, Color)
cornerToColors URF = (White, Red, Green)
cornerToColors ULF = (White, Orange, Green)
cornerToColors URB = (White, Red, Blue)
cornerToColors ULB = (White, Orange, Blue)
cornerToColors DRF = (Yellow, Red, Green)
cornerToColors DLF = (Yellow, Orange, Green)
cornerToColors DRB = (Yellow, Red, Blue)
cornerToColors DLB = (Yellow, Orange, Blue)

edgeToColor :: EdgePosition -> (Color, Color)
edgeToColor UF = (White, Green)
edgeToColor UB = (White, Blue)
edgeToColor UL = (White, Orange)
edgeToColor UR = (White, Red)
edgeToColor DF = (Yellow, Green)
edgeToColor DB = (Yellow, Blue)
edgeToColor DL = (Yellow, Orange)
edgeToColor DR = (Yellow, Red)
edgeToColor RF = (Red, Green)
edgeToColor LF = (Orange, Green)
edgeToColor RB = (Red, Blue)
edgeToColor LB = (Orange, Blue)

solvedCube :: Cube
solvedCube = Cube solvedCorners solvedEdges

