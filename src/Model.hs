data Cube = Cube Corners Edges

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

data CornerCubie = CornerCubie CornerPosition CornerOrientaion 

data CornerPosition = URF | ULF | URB | ULB | DRF | DLF | DRB | DLB

data CornerOrientaion = CNeutral | CW | CCW 

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

data EdgeCubie = EdgeCubie EdgePosition EdgeOrientation

data EdgePosition = UF | UR | UL | UB | DF | DR | DL | DB | RF | LF | RB | LB

data EdgeOrientation = ENeutral | EFlipped


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


solvedCube :: Cube
solvedCube = Cube solvedCorners solvedEdges

