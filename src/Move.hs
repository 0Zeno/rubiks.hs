module Move (Move(..)) where
    
data Move = U | D | F | B | L | R 
    deriving (Show, Eq) 