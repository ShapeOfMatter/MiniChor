{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Quicksort

## Overview

This example implements the three-way concurrent implementation of quicksort.

## Execution

This choreography will sort the list `[1, 6, 5, 3, 4, 2, 7, 8]`. It uses the local backend and distribute the computation over threads.

```bash
cabal run quicksort
[1,2,3,4,5,6,7,8]
```

`Reference.hs` contains a single-threaded reference implementation of the algorithm.
-}

module QuickSort where

import Choreography
import Choreography.Network.Local
import Control.Concurrent.Async (mapConcurrently_)
import CLI (runCLIIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import GHC.TypeLits (KnownSymbol)

reference :: [Int] -> [Int]
reference [] = []
reference (x : xs) = smaller ++ [x] ++ bigger
  where
    smaller = reference [a | a <- xs, a <= x]
    bigger = reference [a | a <- xs, a > x]

$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")

type Participants = ["primary", "worker1", "worker2"]

quicksort ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c, KnownSymbols ps) =>
  Member a ps ->
  Member b ps ->
  Member c ps ->
  Located '[a] [Int] ->
  Choreo ps (Located '[a] [Int])
quicksort a b c lst = do
  isEmpty <- locally1 a (singleton, lst) \l -> pure (null l)
  broadcast (a, isEmpty) >>= \case
    True -> do
      a `locally` pure []
    False -> do
      sm <- congruently1 (a @@ nobody) (refl, lst) \(x:xs) -> [i | i <- xs, i <= x]
      smaller <- (a, sm) ~> b @@ nobody
      smaller' <- quicksort b c a smaller
      smaller'' <- (b, smaller') ~> a @@ nobody
      bg <- congruently1 (a @@ nobody) (refl, lst) \(x:xs) -> [i | i <- xs, i > x]
      bigger <- (a, bg) ~> c @@ nobody
      bigger' <- quicksort c a b bigger
      bigger'' <- (c, bigger') ~> a @@ nobody
      congruently3
        (a @@ nobody)
        (refl, smaller'')
        (refl, lst)
        (refl, bigger'')
        \early fulcrum late -> early ++ [head fulcrum] ++ late

mainChoreo :: Choreo Participants ()
mainChoreo = do
  lst <- primary `locally` return [1, 6, 5, 3, 4, 2, 7, 8]
  sorted <- quicksort primary worker1 worker2 lst
  void $ locally1 primary (primary, sorted) (liftIO . print)
  return ()

main :: IO ()
main = do
  config <- mkLocalConfig locations
  mapConcurrently_ (runCLIIO . runChoreography config mainChoreo) locations
  return ()
  where
    locations = ["primary", "worker1", "worker2"]
