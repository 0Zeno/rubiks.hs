module Display where

import Model

getCornerSticker :: CornerCubie ->  Int -> Color
getCornerSticker (CornerCubie pos ori) face =
    let (c0, c1, c2) = cornerToColors pos
        colors = [c0, c1, c2]
        offset = case ori of
            CNeutral -> 0
            CCW -> 1
            CW -> 2
    in colors !! mod (face + offset) 3


getEdgeSticker :: EdgeCubie -> Int -> Color
getEdgeSticker (EdgeCubie pos ori) face =
    let (c0, c1) = edgeToColor pos
        colors = [c0, c1]
        offset = case ori of 
            ENeutral -> 0
            EFlipped -> 1
    in colors !! mod (face + offset) 2


upFace :: Cube -> [Color]
upFace (Cube corners edges) = 
    [getCornerSticker (ulb corners) 0, 
    getEdgeSticker (ub edges) 0, 
    getCornerSticker (urb corners) 0,
    getEdgeSticker (ul edges) 0,
    White,
    getEdgeSticker (ur edges) 0,
    getCornerSticker (ulf corners) 0,
    getEdgeSticker (uf edges) 0,
    getCornerSticker (urf corners) 0
    ]


downFace:: Cube -> [Color]
downFace (Cube corners edges) =
    [getCornerSticker (dlb corners) 0, 
    getEdgeSticker (db edges) 0, 
    getCornerSticker (drb corners) 0,
    getEdgeSticker (dl edges) 0,
    Yellow,
    getEdgeSticker (dr edges) 0,
    getCornerSticker (dlf corners) 0,
    getEdgeSticker (df edges) 0,
    getCornerSticker (drf corners) 0
    ]

frontFace :: Cube -> [Color]
frontFace (Cube corners edges) =
    [getCornerSticker (ulf corners) 2, 
    getEdgeSticker (uf edges) 1, 
    getCornerSticker (urf corners) 2,
    getEdgeSticker (uf edges) 1,
    Green,
    getEdgeSticker (uf edges) 1,
    getCornerSticker (ulf corners) 2,
    getEdgeSticker (uf edges) 1,
    getCornerSticker (urf corners) 2
    ]

backFace :: Cube -> [Color]
backFace (Cube corners edges) =
    [getCornerSticker (ulb corners) 2, 
    getEdgeSticker (ub edges) 1, 
    getCornerSticker (urb corners) 2,
    getEdgeSticker (ub edges) 1,
    Blue,
    getEdgeSticker (ub edges) 1,
    getCornerSticker (ulb corners) 2,
    getEdgeSticker (ub edges) 1,
    getCornerSticker (urb corners) 2
    ]

rightFace :: Cube -> [Color]
rightFace (Cube corners edges) =
    [getCornerSticker (urb corners) 1, 
    getEdgeSticker (ur edges) 1, 
    getCornerSticker (urb corners) 1,
    getEdgeSticker (rb edges) 0,
    Red,
    getEdgeSticker (rf edges) 0,
    getCornerSticker (urb corners) 1,
    getEdgeSticker (dr edges) 1,
    getCornerSticker (urb corners) 1
    ]

leftFace :: Cube -> [Color]
leftFace (Cube corners edges) =
    [getCornerSticker (ulb corners) 1, 
    getEdgeSticker (ul edges) 1, 
    getCornerSticker (ulb corners) 1,
    getEdgeSticker (lb edges) 0,
    Orange,
    getEdgeSticker (lf edges) 0,
    getCornerSticker (ulb corners) 1,
    getEdgeSticker (dl edges) 1,
    getCornerSticker (ulb corners) 1
    ]