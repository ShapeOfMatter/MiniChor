{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: merge sort

## Overview

This example implements the three-way concurrent implementation of merge sort shown in [Object-Oriented Choreographic Programming](https://arxiv.org/abs/2005.09520).

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It requires three locations: `primary`, `worker1`, and `worker2`.

```bash
# start two workers on separate terminals
cabal run mergesort worker1
cabal run mergesort worker2
# start primary on another terminal
cabal run mergesort primary
[1,2,3,4,5,6,7,8]
```
-}

module MergeSort where

import Choreography
import Choreography.Network.Http
import CLI (runCLIIO)
import Control.Monad.IO.Class (liftIO)
import GHC.TypeLits (KnownSymbol)
import System.Environment

divide :: [a] -> ([a], [a])
divide xs = splitAt lhx xs
  where
    lhx = length xs `div` 2

$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")

type Participants = ["primary", "worker1", "worker2"]

sort ::
  ( KnownSymbol a,
    KnownSymbol c,
    KnownSymbol b,
    KnownSymbols ps
  ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[a] [Int] ->
  Choreo ps (Located '[a] [Int])
sort a b c lst = do
  let condition = ((> 1) . length) <$> lst 
  broadcast First (a @@ nobody) condition >>= \case
    True -> do
      -- _ <- a `locally` \un -> do return $ length (un singleton lst) `div` 2  -- IDK what this was for...
      let divided = divide <$> lst
          l = fst <$> divided
          r = snd <$> divided
      l' <- (a, l) ~> b @@ nobody
      r' <- (a, r) ~> c @@ nobody
      ls' <- sort b c a l'
      rs' <- sort c a b r'
      merge a b c ls' rs'
    False -> do
      return lst

merge ::
  ( KnownSymbol a,
    KnownSymbol c,
    KnownSymbol b,
    KnownSymbols ps
  ) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[b] [Int] ->
  Located '[c] [Int] ->
  Choreo ps (Located '[a] [Int])
merge a b c lhs rhs = do
  let lhsHasElements = (not . null) <$> lhs
  broadcast First (b @@ nobody) lhsHasElements >>= \case
    True -> do
      let rhsHasElements = (not . null) <$> rhs
      broadcast First (c @@ nobody) rhsHasElements >>= \case
        True -> do
          let rhsHeadAtC = head <$> rhs
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b @@ nobody
          let takeLhs = (\lhs' rhsH -> head lhs' <= rhsH) <$> lhs <*> rhsHeadAtB
          broadcast First (b @@ nobody) takeLhs >>= \case
            True -> do
              -- take (head lhs) and merge the rest
              let lhs' = tail <$> lhs
              merged <- merge a b c lhs' rhs
              let lhsHeadAtB = head <$> lhs
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a @@ nobody
              pure $ (:) <$> lhsHeadAtA <*> merged
            False -> do
              -- take (head rhs) and merge the rest
              let rhs' = tail <$> rhs
              merged <- merge a b c lhs rhs'
              let rhsHeadAtC' = head <$> rhs
              rhsHeadAtA <- (c, rhsHeadAtC') ~> a @@ nobody
              pure $ (:) <$> rhsHeadAtA <*> merged
        False -> do
          (b, lhs) ~> a @@ nobody
    False -> do
      (c, rhs) ~> a @@ nobody

mainChoreo :: Choreo Participants ()
mainChoreo = do
  lst <- primary `locally` return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  locallyM_ primary $ (liftIO . print) <$> sorted
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "primary" -> runCLIIO $ runChoreography config mainChoreo "primary"
    "worker1" -> runCLIIO $ runChoreography config mainChoreo "worker1"
    "worker2" -> runCLIIO $ runChoreography config mainChoreo "worker2"
    _ -> error "unknown worker"
  return ()
  where
    config =
      mkHttpConfig
        [ ("primary", ("localhost", 3000)),
          ("worker1", ("localhost", 4000)),
          ("worker2", ("localhost", 5000))
        ]
