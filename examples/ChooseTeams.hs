{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChooseTeams where

import CLI
import Choreography
import Control.Monad (void)
import Data (TestArgs, reference)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Test.QuickCheck (Arbitrary, arbitrary)

newtype Args = Args
  { choices :: (Int, Int)
  }
  deriving (Eq, Show, Read)

instance TestArgs Args ([Int], [Int], [Int], [Int], [Int]) where
  reference Args {choices = (c1, c2)} = ([c1, c2], [], [c1, c2], [], [c1, c2])

instance Arbitrary Args where
  arbitrary = Args <$> arbitrary

chooseTeams :: [LocTm] -> ([LocTm], [LocTm]) -- Probably could find it off-the-shelf somewhere...
chooseTeams [] = ([], [])
chooseTeams (a : as) =
  let (t1, t2) = chooseTeams as
   in (t2, a : t1)

-- the game is just red-team sending numbers to blue team.
game :: forall players . (KnownSymbols players) => Choreo players ()
game = do
  let players = allOf @players
  let (red, blue) = chooseTeams $ toLocs players
  ns <- fanIn players \p ->
    if toLocTm p `elem` red
      then (p, locally' $ Just <$> getInput @Int "A number to send:") ~> players
      else enclave players $ return Nothing
  numbers <- ns
  void $ fanOut \p -> 
    if (toLocTm p `elem` blue)
      then do
        locally p (putOutput "Numbers recieved:" . catMaybes . toList $ numbers)
      else 
        pure $ pure ()
