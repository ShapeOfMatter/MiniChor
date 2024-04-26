{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
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

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Local
import Control.Concurrent.Async (mapConcurrently_)
import GHC.TypeLits (KnownSymbol)
import Logic.Propositional (introAnd)
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
  done <- a `locally` \un -> return $ un explicitMember n1 < 10 || un explicitMember n2 < 10
  broadcastCond
    (explicitMember `introAnd` a, done)
    \case
      True -> do
        a `locally` \un -> return $ un explicitMember n1 * un explicitMember n2
      False -> do
        x <- a `locally` \un -> return $ f (un explicitMember n1) (un explicitMember n2)
        l1' <- (a, \un -> return $ l1 (un explicitMember x)) ~~> b @@ nobody
        l2' <- (a, \un -> return $ l2 (un explicitMember x)) ~~> b @@ nobody
        h1' <- (a, \un -> return $ h1 (un explicitMember x)) ~~> c @@ nobody
        h2' <- (a, \un -> return $ h2 (un explicitMember x)) ~~> c @@ nobody
        z0' <- karatsuba b c a l1' l2'
        z0 <- (explicitMember `introAnd` b, z0') ~> a @@ nobody
        z2' <- karatsuba c a b h1' h2'
        z2 <- (explicitMember `introAnd` c, z2') ~> a @@ nobody
        s1 <- a `locally` \un -> return $ l1 (un explicitMember x) + h1 (un explicitMember x)
        s2 <- a `locally` \un -> return $ l2 (un explicitMember x) + h2 (un explicitMember x)
        z1' <- karatsuba a b c s1 s2
        z1 <- a `locally` \un -> return $ un explicitMember z1' - un explicitMember z2 - un explicitMember z0
        a `locally` \un -> return let s = splitter (un explicitMember x)
                                  in (un explicitMember z2 * s * s) + (un explicitMember z1 * s) + un explicitMember z0
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
  n1 <- primary `locally` \_ -> return n1'
  n2 <- primary `locally` \_ -> return n2'
  result <- karatsuba primary worker1 worker2 n1 n2
  primary `locally_` \un -> do
    print (un primary result)

main :: IO ()
main = do
  [n1, n2] <- map read <$> getArgs
  config <- mkLocalConfig locations
  mapConcurrently_ (runChoreography config (mainChoreo n1 n2)) locations
  return ()
  where
    locations = ["primary", "worker1", "worker2"]
