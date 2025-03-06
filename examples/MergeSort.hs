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
import Control.Monad (void)
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
  Choreo ps IO (Located '[a] [Int])
sort a b c lst = do
  condition <- congruently1 (a @@ nobody) (refl, lst) ((> 1) . length)
  broadcast (a, condition) >>= \case
    True -> do
      -- _ <- a `locally` \un -> do return $ length (un singleton lst) `div` 2  -- IDK what this was for...
      divided <- congruently1 (a @@ nobody) (refl, lst) divide
      l <- congruently1 (a @@ nobody) (refl, divided) fst
      r <- congruently1 (a @@ nobody) (refl, divided) snd
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
  Choreo ps IO (Located '[a] [Int])
merge a b c lhs rhs = do
  lhsHasElements <- congruently1 (b @@ nobody) (refl, lhs) (not . null)
  broadcast (b, lhsHasElements) >>= \case
    True -> do
      rhsHasElements <- congruently1 (c @@ nobody) (refl, rhs) (not . null)
      broadcast (c, rhsHasElements) >>= \case
        True -> do
          rhsHeadAtC <- congruently1 (c @@ nobody) (refl, rhs) head
          rhsHeadAtB <- (c, rhsHeadAtC) ~> b @@ nobody
          takeLhs <- congruently2 (b @@ nobody) (refl, lhs) (refl, rhsHeadAtB) \lhs' rhsH -> head lhs' <= rhsH
          broadcast (b, takeLhs) >>= \case
            True -> do
              -- take (head lhs) and merge the rest
              lhs' <- congruently1 (b @@ nobody) (refl, lhs) tail
              merged <- merge a b c lhs' rhs
              lhsHeadAtB <- congruently1 (b @@ nobody) (refl, lhs) head
              lhsHeadAtA <- (b, lhsHeadAtB) ~> a @@ nobody
              congruently2 (a @@ nobody) (refl, lhsHeadAtA) (refl, merged) (:)
            False -> do
              -- take (head rhs) and merge the rest
              rhs' <- congruently1 (c @@ nobody) (refl, rhs) tail
              merged <- merge a b c lhs rhs'
              rhsHeadAtC' <- congruently1 (c @@ nobody) (refl, rhs) head
              rhsHeadAtA <- (c, rhsHeadAtC') ~> a @@ nobody
              congruently2 (a @@ nobody) (refl, rhsHeadAtA) (refl, merged) (:)
        False -> do
          (b, lhs) ~> a @@ nobody
    False -> do
      (c, rhs) ~> a @@ nobody

mainChoreo :: Choreo Participants IO ()
mainChoreo = do
  lst <- primary `locally` return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- sort primary worker1 worker2 lst
  void $ locally1 primary (primary, sorted) print
  return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "primary" -> runChoreography config mainChoreo "primary"
    "worker1" -> runChoreography config mainChoreo "worker1"
    "worker2" -> runChoreography config mainChoreo "worker2"
    _ -> error "unknown worker"
  return ()
  where
    config =
      mkHttpConfig
        [ ("primary", ("localhost", 3000)),
          ("worker1", ("localhost", 4000)),
          ("worker2", ("localhost", 5000))
        ]
