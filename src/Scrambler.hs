module Scrambler (scramble) where
import Move (Move(..))
import System.Random
{-
generate a random state scramble - I use 30 rotations as a default as I check for non-valid rotations.

code makes sure there are no self cancelling rotations .. eg .. "R" cannot be followed by "R'"

no consecutive opposites rotations .. eg .. "L" cannot be followed by "R" ... "U" cannot be followed by "D".

double rotations are allowed but no more than 3 or it becomes a prime move while rule 3 applies.

once it is scrambled and we have a "state" - we pass it through Kociemba algorithm to solve it ..

once solved - that solution string becomes a SCRAMBLE!

-}
scramble :: Int -> IO ()
scramble numberOfMoves = do
    moves <- mapM (\_ -> getRandomMove) [0..numberOfMoves]
    print(moves)

getRandomMove :: IO (Move)
getRandomMove = do
    randomNum <- randomRIO (0, 5) :: IO Int
    return (numberToMove randomNum)


numberToMove :: Int -> Move
numberToMove 0 = U
numberToMove 1 = D
numberToMove 2 = F 
numberToMove 3 = B 
numberToMove 4 = L
numberToMove 5 = R
