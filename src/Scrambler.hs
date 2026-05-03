module Scrambler (scramble, generateMoves) where
import Move (Move(..))
import System.Random
import Helper (inverse)


scramble :: Int -> IO [Move]
scramble n = generateMoves (Nothing, Nothing) n

generateMoves :: (Maybe Move, Maybe Move) -> Int -> IO [Move]
generateMoves (_, _) 0 = return []
generateMoves (prev1, prev2) n = do
    randomMove <- getRandomMove
    if prev1 == Just randomMove && prev2 == Just randomMove
        then generateMoves (prev1, prev2) n
    else if opposites prev2 (Just randomMove)
        then generateMoves (prev1, prev2) n
    else do
        rest <- generateMoves (prev2, Just randomMove) (n-1)
        return (randomMove : rest)


getRandomMove :: IO Move
getRandomMove = do
    randomNum <- randomRIO (0, 17) :: IO Int
    return (numberToMove randomNum)

opposites :: Maybe Move -> Maybe Move -> Bool
opposites (Just m1) ( Just m2 ) = m1 == inverse m2
opposites _ _ = False

numberToMove :: Int -> Move
numberToMove 0 = U
numberToMove 1 = D
numberToMove 2 = F
numberToMove 3 = B
numberToMove 4 = L
numberToMove 5 = R
numberToMove 6 = Up
numberToMove 7 = Dp
numberToMove 8 = Fp
numberToMove 9 = Bp
numberToMove 10 = Lp
numberToMove 11 = Rp
numberToMove 12 = U2
numberToMove 13 = D2
numberToMove 14 = F2
numberToMove 15 = B2
numberToMove 16 = L2
numberToMove 17 = R2
numberToMove _ = error "numberToMove does not allow this"