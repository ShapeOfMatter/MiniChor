{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Example: diffie-hellman key exchange

## Overview

This example implements the [diffie-hellman key exchange protocol](https://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange).

In this example, two locations, `alice` and `bob`, exchange the secret key without sending the key over the network.

## Execution

To run this example, you will need to run two locations (alice and bob) at the same time. Alice initiates the exchange, and Bob waits for Alice. When both locations are ready, press "enter" on Alice's terminal to start the protocol.

```
> cabal run diffiehellman bob
waiting for alice to initiate key exchange

# on a different terminal
> cabal run diffiehellman alice
enter to start key exchange...
[Enter]

# Alice's terminal
alice's shared key: 1544

# Bob's terminal
bob's shared key: 1544
```

This sample uses [`System.Random`](https://hackage.haskell.org/package/random-1.2.1.1/docs/System-Random.html) and will generate different keys at each invocation.
-}

module DiffieHellman where

import CLI
import Choreography
import Choreography.Network.Http
import System.Environment
import System.Random

-- helper functions around prime number
-- https://nulldereference.wordpress.com/2012/02/04/generating-prime-numbers-with-haskell/
divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors x = 1 : [y | y <- [2 .. (x `div` 2)], x `mod` y == 0] ++ [x]

isPrime :: Integer -> Bool
isPrime x = divisors x == [1, x]

primeNums :: [Integer]
primeNums = [x | x <- [2 ..], isPrime x]

$(mkLoc "alice")
$(mkLoc "bob")

type Participants = ["alice", "bob"]

diffieHellman ::
  Choreo Participants ()
diffieHellman = do
  -- wait for alice to initiate the process
  _ <- alice `locally` getstr "enter to start key exchange..."
  bob `locally_` putNote "waiting for alice to initiate key exchange"

  -- alice picks p and g and sends them to bob
  pa <-
    alice `locally` do
      x <- randomRIO (200, 1000 :: Int)
      return $ primeNums !! x
  pb <- (alice, pa) ~> bob @@ nobody
  ga <- locallyM alice (randomRIO . (10,) <$> pa)
  gb <- (alice, ga) ~> bob @@ nobody

  -- alice and bob select secrets
  a <- alice `locally` randomRIO (200, 1000 :: Integer)
  b <- bob `locally` randomRIO (200, 1000 :: Integer)

  -- alice and bob computes numbers that they exchange
  let powMod x y z = x ^ y `mod` z
  let a' = powMod <$> ga <*> a <*> pa
  let b' = powMod <$> gb <*> b <*> pb

  -- exchange numbers
  a'' <- (alice, a') ~> bob @@ nobody
  b'' <- (bob, b') ~> alice @@ nobody

  -- compute shared key
  locallyM_ alice $ putOutput "alice's shared key:" <$> (powMod <$> b'' <*> a <*> pa)
  locallyM_ bob $ putOutput "bob's shared key:" <$> (powMod <$> a'' <*> b <*> pb)

main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "alice" -> runCLIIO $ runChoreography config diffieHellman "alice"
    "bob" -> runCLIIO $ runChoreography config diffieHellman "bob"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("alice", ("localhost", 5000)),
          ("bob", ("localhost", 5001))
        ]
