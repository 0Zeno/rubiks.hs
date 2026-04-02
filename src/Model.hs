module Model (Cube(..), Face(..), Row(..), Color(..)) where

data Cube = 
    Cube {
        up :: Face,
        down :: Face, 
        front :: Face, 
        back :: Face, 
        left :: Face, 
        right :: Face
    }
    deriving (Eq) 


data Face = 
    Face Row Row Row
    deriving (Eq)



data Row = 
    Row Color Color Color
    deriving (Eq)


data Color =
    Red | Blue | Yellow | White | Green | Orange
    deriving (Eq)
