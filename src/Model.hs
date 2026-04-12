module Model where

data Cube = 
    Cube Corners Edges
    deriving (Show)

data Color = 
    Red | Yellow | Green | Blue | White | Orange    
instance Show Color where
    show :: Color -> String
    show Red = "R"
    show Blue = "B"
    show Yellow = "Y"
    show White = "W"
    show Green = "G"
    show Orange = "O"

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

instance Show CornerCubie where
    show :: CornerCubie -> String
    show (CornerCubie pos ori) = show pos ++ " " ++ show ori

data CornerPosition =
    URF | ULF | URB | ULB | DRF | DLF | DRB | DLB
    deriving (Show)

data CornerOrientaion =
    CNeutral | CW | CCW 
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

instance Show EdgeCubie where
    show :: EdgeCubie -> String
    show  (EdgeCubie pos ori) = show pos ++ "  " ++ show ori

data EdgePosition = 
    UF | UR | UL | UB | DF | DR | DL | DB | RF | LF | RB | LB
    deriving (Show)                                                               

data EdgeOrientation = 
    ENeutral | EFlipped
    deriving (Show)                                                               


solvedCorners :: Corners
solvedCorners = Corners
    { urf = CornerCubie URF CNeutral
    , ulf = CornerCubie ULF CNeutral
    , urb = CornerCubie URB CNeutral
    , ulb = CornerCubie ULB CNeutral
    , drf = CornerCubie DRF CNeutral
    , dlf = CornerCubie DLF CNeutral
    , drb = CornerCubie DRB CNeutral
    , dlb = CornerCubie DLB CNeutral
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

