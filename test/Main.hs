module Main (main) where
import Move (Move(..))
import Scrambler (generateMoves)
import Test.QuickCheck

instance Arbitrary Move where
      arbitrary = elements [U, D, F, B, L, R]

main :: IO ()
main = do
    quickCheck $ \n -> ioProperty $ do
        moves <- generateMoves (Nothing, Nothing) (abs n + 1)
        return (noTriplet moves)

noTriplet :: [Move] -> Bool
noTriplet [] = True
noTriplet [_] = True
noTriplet [_,_] = True
noTriplet (a:b:c:rest) = if a == c && b == c then False else noTriplet rest 
