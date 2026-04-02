module Main (main) where
import Move (Move(..))
import Scrambler (generateMoves, opposites)
import Test.QuickCheck

instance Arbitrary Move where
      arbitrary = elements [U, D, F, B, L, R]

main :: IO ()
main = do
    quickCheck $ \n -> ioProperty $ do
        moves <- generateMoves (Nothing, Nothing) (abs n + 1)
        return (noTriplet moves)
    quickCheck $ \n -> ioProperty $ do
      moves <- generateMoves (Nothing, Nothing) (abs n + 1)
      return (noConsecutiveOpposites moves)

noTriplet :: [Move] -> Bool
noTriplet [] = True
noTriplet [_] = True
noTriplet [_,_] = True
noTriplet (a:b:c:rest) = if a == c && b == c then False else noTriplet rest 

noConsecutiveOpposites :: [Move] -> Bool
noConsecutiveOpposites [] = True
noConsecutiveOpposites [_] = True
noConsecutiveOpposites (a:b:rest) = if opposites (Just a) (Just b) then False else noConsecutiveOpposites (b:rest)
