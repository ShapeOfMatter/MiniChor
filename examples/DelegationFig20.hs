{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DelegationFig20 where

{-
let choice : ()+()@[alice, bob] = com[alice][alice, bob] alices_choice;
let query : Query@[alice] = case[alice, bob] choice of
Inl _ => com[bob][alice] bobs_query;
Inr _ => alices_query;
let answerer : (Query@[carroll] -> Response@[carroll])@[carroll] = carrolls_func;
let response = com[carroll][bob, alice] (answerer (com[alice][carroll] query));
case[alice, bob] choice of
Inl _ => bobs_terminal response;
Inr _ => alices_terminal response;
 -}

import CLI
import Choreography
import Data (TestArgs, reference)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryPrintableChar, elements, listOf1)

$(mkLoc "alice")
$(mkLoc "bob")
$(mkLoc "carroll")

type Participants = ["alice", "bob", "carroll"]

data Args = Args
  { choice :: Bool,
    aliceQ :: String,
    bobQ :: String,
    carrollF :: String
  }
  deriving (Eq, Show, Read)

data Result = Result
  { aliceR :: [String],
    bobR :: [String],
    carrollR :: [String]
  }
  deriving (Eq, Show, Read)

instance TestArgs Args Result where
  reference Args {choice, aliceQ, bobQ, carrollF} =
    let f = fromMaybe carrollsDefault $ carrollF `lookup` carrollsFunctions
        result = f $ if choice then aliceQ else bobQ
     in Result {aliceR = [result | choice], bobR = [result | not choice], carrollR = []}

instance Arbitrary Args where
  arbitrary =
    Args
      <$> arbitrary
      <*> listOf1 arbitraryPrintableChar -- Avoiding wierd behavior of CLI on empty outputs :(
      <*> listOf1 arbitraryPrintableChar
      <*> elements (fst <$> carrollsFunctions)

carrollsFunctions :: [(String, String -> String)]
carrollsFunctions =
  [ ("reverse", reverse),
    ("alphabetize", sort)
  ]

carrollsDefault :: String -> String
carrollsDefault = const "No Handler"

mainCho :: Choreo Participants ()
mainCho = do
  choice <- (alice, locally' $ getInput "Alice's choice:") ~> alice @@ bob @@ nobody
  query <- cond (alice @@ bob @@ nobody, explicitSubset, choice) (
      \case
        False -> (bob, locally' $ getstr "Bob's query:") ~> alice @@ nobody
        True -> alice `locally` getstr "Alice's query:"
    ) >>= enclaveTo (alice @@ bob @@ nobody) (First @@ nobody)
  answerer <-
    carroll `locally` do
      handlerName <- getstr "Carrol's function (reverse or alphabetize):"
      return $ fromMaybe carrollsDefault $ handlerName `lookup` carrollsFunctions
  query' <- (alice, query) ~> carroll @@ nobody
  let resp = answerer <*> query'
  response <- (carroll, resp) ~> alice @@ bob @@ nobody
  let finalize = putstr "Recieved:" <$> response
  (_ :: Located '["alice", "bob"] ()) <- cond (explicitSubset, explicitSubset, choice) \case
    False -> othersForget explicitSubset explicitSubset finalize >>= locallyM_ bob
    True -> othersForget explicitSubset explicitSubset finalize >>= locallyM_ alice
  return ()
