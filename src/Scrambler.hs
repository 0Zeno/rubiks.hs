module Scrambler (scramble, generateMoves, opposites) where
import Move (Move(..))
import System.Random
{-
code makes sure there are no self cancelling rotations .. eg .. "R" cannot be followed by "R'"

-}

scramble :: Int -> IO ()
scramble n = do
    moves <- generateMoves (Nothing, Nothing) n
    print (moves)

generateMoves :: (Maybe Move, Maybe Move) -> Int -> IO [Move]
generateMoves (_, _) 0 = return []
generateMoves (prev1, prev2) n = do
    randomMove <- getRandomMove

    -- no more than 3 identical moves or it becomes a prime.
    if prev1 == Just randomMove && prev2 == Just randomMove
        then generateMoves (prev1, prev2) n
    -- no consecutive opposites rotations eg. "L" cannot be followed by "R". 
    else if opposites prev2 (Just randomMove)
        then generateMoves (prev1, prev2) n 
    else do
        rest <- generateMoves (prev2, Just randomMove) (n-1)
        return (randomMove : rest) 


getRandomMove :: IO Move
getRandomMove = do
    randomNum <- randomRIO (0, 5) :: IO Int
    return (numberToMove randomNum)

opposite :: Move -> Move
opposite U = D
opposite D = U 
opposite L = R 
opposite R = L 
opposite B = F 
opposite F = B 

opposites :: Maybe Move -> Maybe Move -> Bool
opposites (Just m1) ( Just m2 ) = m1 == opposite m2
opposites _ _ = False

numberToMove :: Int -> Move
numberToMove 0 = U
numberToMove 1 = D
numberToMove 2 = F 
numberToMove 3 = B 
numberToMove 4 = L
numberToMove 5 = R
