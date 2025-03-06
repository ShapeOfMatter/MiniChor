{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Karatsuba Fast Multiplication

## Overview

This example implements the three-way concurrent implementation of the Karatsuba fast multiplication algorithm shown in [Object-Oriented Choreographic Programming](https://arxiv.org/abs/2005.09520).

## Execution

The executable takes two integers as arguments and returns their product. It uses the local backend and distribute the computation over threads.

```bash
cabal run karatsuba 100 200
20000
```

`Reference.h` contains a single-threaded reference implementation of the algorithm.
-}

module Karatsuba where

import Choreography
import Choreography.Network.Local
import Control.Monad (void)
import Control.Concurrent.Async (mapConcurrently_)
import GHC.TypeLits (KnownSymbol)
import System.Environment

reference :: Integer -> Integer -> Integer
reference n1 n2 =
  if n1 < 10 || n2 < 10
    then n1 * n2
    else result
  where
    log10 :: Integer -> Double
    log10 = logBase 10 . fromIntegral
    m = max (log10 n1) (log10 n2) + 1
    m2 :: Integer = floor (m / 2)
    splitter = 10 ^ m2
    h1 = n1 `div` splitter
    l1 = n1 `mod` splitter
    h2 = n2 `div` splitter
    l2 = n2 `mod` splitter
    z0 = reference l1 l2
    z2 = reference h1 h2
    z1 = reference (l1 + h1) (l2 + h2) - z2 - z0
    result = z2 * splitter * splitter + z1 * splitter + z0

$(mkLoc "primary")
$(mkLoc "worker1")
$(mkLoc "worker2")

type Participants = ["primary", "worker1", "worker2"]

data KaratsubaNums = KaratsubaNums
  { splitter :: Integer,
    h1 :: Integer,
    h2 :: Integer,
    l1 :: Integer,
    l2 :: Integer
  }

karatsuba ::
  (KnownSymbol a, KnownSymbol b, KnownSymbol c) =>
  Member a Participants ->
  Member b Participants ->
  Member c Participants ->
  Located '[a] Integer ->
  Located '[a] Integer ->
  Choreo Participants IO (Located '[a] Integer)
karatsuba a b c n1 n2 = do
  done <- congruently2 (a @@ nobody) (refl, n1) (refl, n2) \n1' n2' -> n1' < 10 || n2' < 10
  broadcast (a, done)
    >>= \case
      True -> congruently2 (a @@ nobody) (refl, n1) (refl, n2) (*)
      False -> do
        x <- congruently2 (a @@ nobody) (refl, n1) (refl, n2) f
        l1' <- congruently1 (a @@ nobody) (refl, x) l1 >>= ((~> b @@ nobody) . (a,))  -- this gets less dumb if located is a functor!
        l2' <- congruently1 (a @@ nobody) (refl, x) l2 >>= ((~> b @@ nobody) . (a,))
        h1' <- congruently1 (a @@ nobody) (refl, x) h1 >>= ((~> c @@ nobody) . (a,))  -- this gets less dumb if located is a functor!
        h2' <- congruently1 (a @@ nobody) (refl, x) h2 >>= ((~> c @@ nobody) . (a,))  -- this gets less dumb if located is a functor!
        z0' <- karatsuba b c a l1' l2'
        z0 <- (b, z0') ~> a @@ nobody
        z2' <- karatsuba c a b h1' h2'
        z2 <- (c, z2') ~> a @@ nobody
        s1 <- congruently1 (a @@ nobody) (refl, x) \x' -> l1 x' + h1 x'
        s2 <- congruently1 (a @@ nobody) (refl, x) \x' -> l2 x' + h2 x'
        z1' <- karatsuba a b c s1 s2
        z1 <- congruently3 (a @@ nobody) (refl, z0) (refl, z1') (refl, z2) \z0a z1a z2a -> z1a - z2a - z0a
        enclave (a @@ nobody) do
          s <- splitter <$> naked refl x
          z2a <- naked refl z2
          z1a <- naked refl z1
          z0a <- naked refl z0
          pure $ (z2a * s * s) + (z1a * s) + z0a
        where
          f n1' n2' = KaratsubaNums {splitter = splitter, h1 = h1, l1 = l1, h2 = h2, l2 = l2}
            where
              log10 :: Integer -> Double
              log10 = logBase 10 . fromIntegral
              m = max (log10 n1') (log10 n2') + 1
              m2 :: Integer = floor (m / 2)
              splitter = 10 ^ m2
              h1 = n1' `div` splitter
              l1 = n1' `mod` splitter
              h2 = n2' `div` splitter
              l2 = n2' `mod` splitter

mainChoreo :: Integer -> Integer -> Choreo Participants IO ()
mainChoreo n1' n2' = do
  n1 <- primary `locally` pure n1'
  n2 <- primary `locally` pure n2'
  result <- karatsuba primary worker1 worker2 n1 n2
  void $ locally1 primary (primary, result) print

main :: IO ()
main = do
  [n1, n2] <- map read <$> getArgs
  config <- mkLocalConfig locations
  mapConcurrently_ (runChoreography config (mainChoreo n1 n2)) locations
  return ()
  where
    locations = ["primary", "worker1", "worker2"]
