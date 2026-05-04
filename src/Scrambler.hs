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
    if sameFace prev2 (Just randomMove)
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

sameFace :: Maybe Move -> Maybe Move -> Bool
sameFace (Just m1) (Just m2) = faceOf m1 == faceOf m2
sameFace _ _ = False

faceOf :: Move -> Int
faceOf U  = 0; faceOf Up = 0; faceOf U2 = 0
faceOf D  = 1; faceOf Dp = 1; faceOf D2 = 1
faceOf F  = 2; faceOf Fp = 2; faceOf F2 = 2
faceOf B  = 3; faceOf Bp = 3; faceOf B2 = 3
faceOf L  = 4; faceOf Lp = 4; faceOf L2 = 4
faceOf R  = 5; faceOf Rp = 5; faceOf R2 = 5

opposites :: Maybe Move -> Maybe Move -> Bool
opposites (Just m1) (Just m2) = m1 == inverse m2
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