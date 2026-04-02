module Display () where

import Model 

instance Show Cube where 
    show (Cube up down front back left right) = 
        emptySpace ++ firstRow up ++ "\n" ++
        emptySpace ++ secondRow up ++ "\n" ++
        emptySpace ++ thirdRow up ++ "\n" ++
        
        firstRow left ++ " " ++ firstRow front ++ " " ++ firstRow right ++ " " ++ firstRow back ++ "\n" ++
        secondRow left ++ " " ++ secondRow front ++ " " ++ secondRow right ++ " " ++ secondRow back ++ "\n" ++
        thirdRow left ++ " " ++ thirdRow front ++ " " ++ thirdRow right ++ " " ++ thirdRow back ++ "\n" ++

        emptySpace ++ firstRow down ++ "\n" ++
        emptySpace ++ secondRow down ++ "\n" ++
        emptySpace ++ thirdRow down ++ "\n"

emptySpace :: String
emptySpace = "      "

firstRow :: Face -> String
firstRow (Face r1 _ _) = show r1
    
secondRow :: Face -> String
secondRow (Face _ r2 _) = show r2

thirdRow :: Face -> String
thirdRow (Face _ _ r3) = show r3 

instance Show Row where
    show ( Row c1 c2 c3 ) = show c1 ++ " " ++ show c2 ++ " " ++ show c3

instance Show Color where
    show Red = "R"
    show Blue = "B"
    show Yellow = "Y"
    show White = "W"
    show Green = "G"
    show Orange = "O"