{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardGame where

import CLI
import Choreography
import Control.Monad (void)
import Data (TestArgs, reference)
import Test.QuickCheck (Arbitrary, arbitrary, listOf1)

_modulo :: Int -> Int
_modulo = (`mod` 21)

newtype Card = Card_ Int deriving (Eq, Ord, Read, Show)

card :: Int -> Card
card = Card_ . _modulo

instance Num Card where
  (Card_ a) + (Card_ b) = card $ a + b
  (Card_ a) * (Card_ b) = card $ a * b
  abs = id
  signum = const 1
  fromInteger = card . fromInteger
  negate (Card_ a) = card $ (-1) * a

instance Arbitrary Card where
  arbitrary = Card_ <$> arbitrary

type Win = Bool

data Args = Args
  { deck :: [Card],
    choices :: (Bool, Bool, Bool)
  }
  deriving (Eq, Show, Read)

instance TestArgs Args (Bool, Bool, Bool) where
  reference Args {deck, choices = (c1, c2, c3)} =
    let h11 : h21 : h31 : deck1 = cycle deck -- should just use State
        (h12, deck12) = if c1 then ([h11, head deck1], tail deck1) else ([h11], deck1)
        (h22, deck22) = if c2 then ([h21, head deck12], tail deck12) else ([h21], deck12)
        (h32, deck32) = if c3 then ([h31, head deck22], tail deck22) else ([h31], deck22)
        common = head deck32
        [win1, win2, win3] = (> card 19) . sum . (common :) <$> [h12, h22, h32]
     in (win1, win2, win3)

instance Arbitrary Args where
  arbitrary = Args <$> listOf1 arbitrary <*> arbitrary

{- A simple black-jack-style game. The dealer gives everyone a card, face up. Each player may
 - request a second card. Then the dealer reveals one more card that applies to everyone. Each
 - player individually wins if the sum of their cards (modulo 21) is greater than 19.  -}
game :: forall players . (KnownSymbols players) => Choreo ("dealer" ': players) ()
game = do
  let players = consSuper (refl @players)
      dealer = listedFirst @"dealer" -- listedFirst is just First with the type-arguments rearranged.
      everyone = refl @("dealer" ': players)
  hand1' <- fanIn everyone \(player :: Member player players) -> do
              card1 <- locally dealer (getInput ("Enter random card for " ++ toLocTm player))
              (dealer, card1) ~> everyone
  hand1 <- hand1' -- odd...
  wantsNextCard <- parallel players do
    putNote $ "All cards on the table: " ++ show hand1
    getInput "I'll ask for another? [True/False]"
  hand2 <- fanOut \(player :: Member player players) ->
    enclave (inSuper players player @@ dealer @@ nobody) do
      let dealer' = listedSecond @"dealer"
      choice <- broadcast First (listedFirst @player @@ nobody) (localize player wantsNextCard)
      if choice
        then do
          card2 <- broadcast First (dealer' @@ nobody) (locally' $ getInput (toLocTm player ++ "'s second card:"))
          return [getLeaf hand1 player, card2]
        else return [getLeaf hand1 player]
  tableCard <- (dealer, locally' $ getInput "Enter a single card for everyone:") ~> players
  void $ fanOut \p -> do
    hand <- (:) `lMap` (p @@ nobody, players, tableCard) `lSplat` (First @@ nobody, inSuper players p @@ dealer @@ nobody, localize p hand2)
    locallyM_ (inSuper players p) $ putNote . ("My hand: " ++) . show <$> hand
    locallyM (inSuper players p) $ putOutput "My win result:" . (> card 19) . sum <$> hand
