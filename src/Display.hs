{-# OPTIONS_GHC -Wno-orphans #-}
module Display where

import Model
import Cube

instance Show Color where
    show :: Color -> String
    show Red    = "\ESC[41m  \ESC[0m"
    show Blue   = "\ESC[44m  \ESC[0m"
    show Yellow = "\ESC[48;5;226m  \ESC[0m"
    show White  = "\ESC[47m  \ESC[0m"
    show Green  = "\ESC[42m  \ESC[0m"
    show Orange = "\ESC[48;5;208m  \ESC[0m"

-- instance Show Color where
--     show :: Color -> String
--     show Red    = "R" 
--     show Blue   = "B"
--     show Yellow = "Y"
--     show White  = "W"
--     show Green  = "G"
--     show Orange = "O"

instance Show Cube where
    show :: Cube -> String
    show cube =
        let u = upFace cube
            d = downFace cube
            f = frontFace cube
            b = backFace cube
            l = leftFace cube
            r = rightFace cube
        in unlines
            [ pad ++ row u 0
            , pad ++ row u 1
            , pad ++ row u 2
            , row l 0 ++ " " ++ row f 0 ++ " " ++ row r 0 ++ " " ++ row b 0
            , row l 1 ++ " " ++ row f 1 ++ " " ++ row r 1 ++ " " ++ row b 1
            , row l 2 ++ " " ++ row f 2 ++ " " ++ row r 2 ++ " " ++ row b 2
            , pad ++ row d 0
            , pad ++ row d 1
            , pad ++ row d 2
            ]
      where
        pad = "         "
        row colors n = unwords (map show (take 3 (drop (n * 3) colors)))

getCornerSticker :: CornerCubie -> Int -> Color
getCornerSticker (CornerCubie pos (CornerOri i j k)) face =
    let (c0, c1, c2) = cornerToColors pos
        colors = [c0, c1, c2]
    in colors !! ([i, j, k] !! face)

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
    [ getCornerSticker (ulb corners) 0, getEdgeSticker (ub edges) 0, getCornerSticker (urb corners) 0
    , getEdgeSticker   (ul edges)    0, White,                        getEdgeSticker   (ur edges)    0
    , getCornerSticker (ulf corners) 0, getEdgeSticker (uf edges) 0, getCornerSticker (urf corners) 0
    ]

downFace :: Cube -> [Color]
downFace (Cube corners edges) =
    [ getCornerSticker (dlf corners) 0, getEdgeSticker (df edges) 0, getCornerSticker (drf corners) 0
    , getEdgeSticker   (dl edges)    0, Yellow,                       getEdgeSticker   (dr edges)    0
    , getCornerSticker (dlb corners) 0, getEdgeSticker (db edges) 0, getCornerSticker (drb corners) 0
    ]

frontFace :: Cube -> [Color]
frontFace (Cube corners edges) =
    [ getCornerSticker (ulf corners) 2, getEdgeSticker (uf edges) 1, getCornerSticker (urf corners) 2
    , getEdgeSticker   (lf edges)    1, Green,                        getEdgeSticker   (rf edges)    1
    , getCornerSticker (dlf corners) 2, getEdgeSticker (df edges) 1, getCornerSticker (drf corners) 2
    ]

backFace :: Cube -> [Color]
backFace (Cube corners edges) =
    [ getCornerSticker (urb corners) 2, getEdgeSticker (ub edges) 1, getCornerSticker (ulb corners) 2
    , getEdgeSticker   (rb edges)    1, Blue,                         getEdgeSticker   (lb edges)    1
    , getCornerSticker (drb corners) 2, getEdgeSticker (db edges) 1, getCornerSticker (dlb corners) 2
    ]

rightFace :: Cube -> [Color]
rightFace (Cube corners edges) =
    [ getCornerSticker (urf corners) 1, getEdgeSticker (ur edges) 1, getCornerSticker (urb corners) 1
    , getEdgeSticker   (rf edges)    0, Red,                          getEdgeSticker   (rb edges)    0
    , getCornerSticker (drf corners) 1, getEdgeSticker (dr edges) 1, getCornerSticker (drb corners) 1
    ]

leftFace :: Cube -> [Color]
leftFace (Cube corners edges) =
    [ getCornerSticker (ulb corners) 1, getEdgeSticker (ul edges) 1, getCornerSticker (ulf corners) 1
    , getEdgeSticker   (lb edges)    0, Orange,                       getEdgeSticker   (lf edges)    0
    , getCornerSticker (dlb corners) 1, getEdgeSticker (dl edges) 1, getCornerSticker (dlf corners) 1
    ]
