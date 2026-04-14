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
