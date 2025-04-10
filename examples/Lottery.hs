{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lottery where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Exception (Exception, throwIO)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash (Digest)
import Crypto.Hash qualified as Crypto
import Data (TestArgs, reference)
import Data.Bifunctor (first)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.FiniteField (primeField)
import Data.Foldable (toList)
import GHC.TypeLits (KnownSymbol)
import System.Environment (getArgs)
import System.Random (Random, random, randomIO, randomR, randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Arbitrary "large" prime
newtype Fp = Fp $(primeField 999983) -- AFAICT the hint warning needs a newer version of hlint to go away:
  deriving (Bounded, Enum, Eq, Ord, Num) -- https://github.com/ndmitchell/hlint/issues/1226

instance Read Fp where
  readsPrec _ s = [(Fp (read s), "")]

instance Show Fp where
  show (Fp x) = show x

instance Random Fp where
  random g = toEnum `first` randomR (fromEnum @Fp minBound, fromEnum @Fp maxBound) g
  randomR (l, h) g = toEnum `first` randomR (fromEnum l, fromEnum h) g

instance Arbitrary Fp where
  arbitrary = arbitraryBoundedEnum

data LotteryError = CommitmentCheckFailed deriving (Show)

instance Exception LotteryError

data Args = Args
  { secrets :: (Fp, Fp, Fp, Fp, Fp), -- we lock our test to the case of five clients and three servers
    randomIs :: (Int, Int, Int)
  }
  deriving (Eq, Show, Read)

instance TestArgs Args Fp where
  reference Args {secrets = (c1, c2, c3, c4, c5), randomIs = (s1, s2, s3)} =
    let i = (s1 + s2 + s3) `mod` 5
     in [c1, c2, c3, c4, c5] !! i

instance Arbitrary Args where
  arbitrary =
    Args
      <$> ((,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
      <*> ((,,) <$> arbitrary <*> arbitrary <*> arbitrary)

-- | Federated Lottery example from DPrio https://www.semanticscholar.org/paper/DPrio%3A-Efficient-Differential-Privacy-with-High-for-Keeler-Komlo/ae1b2a4e5beaaa850183ad37e0880bb70ae34f4e
lottery ::
  forall clients servers analyst census _serv1 _serv2 _servTail _client1 _client2 _clientTail.
  ( KnownSymbols clients,
    KnownSymbols servers,
    KnownSymbol analyst,
    (_serv1 ': _serv2 ': _servTail) ~ servers, -- There must be at least be two servers
    (_client1 ': _client2 ': _clientTail) ~ clients -- There must be at least be two clients
  ) =>
  Subset clients census ->
  Subset servers census ->
  Member analyst census ->
  Choreo census ()
lottery clients servers analyst = do
  secret <- parallel clients (getInput @Fp "secret:")
  clientShares <- fanOut \client -> enclave (inSuper clients client @@ nobody) (
      case tySpine @servers of -- I guess this explains to GHC that we have KnownSymbols _servTail? IDK
          TyCons -> do
            sec <- localize client secret
            locally' do freeShares <- liftIO $ sequence $ pure $ randomIO @Fp
                        return $ (sec - sum freeShares) `qCons` freeShares
    )
  serverShares <-
    fanOut
      ( \server ->
          -- Probably I've already got a nicer way to write this on hand; idk.
          fanIn
            (inSuper servers server @@ nobody)
            ( \client -> do
                let serverShare = (`getLeaf` server) <$> localize client clientShares
                (inSuper clients client, serverShare) ~> inSuper servers server @@ nobody
            )
      )
  -- 1) Each server selects a random number; τ is some multiple of the number of clients.
  ρ <- parallel servers (getInput "A random number from 1 to τ:")
  -- Salt value
  ψ <- parallel servers (randomRIO (2 ^ (18 :: Int), 2 ^ (20 :: Int)))
  -- 2) Each server computes and publishes the hash α = H(ρ, ψ) to serve as a commitment
  α <- fanOut \server -> enclave (inSuper servers server @@ nobody) (hash <$> localize server ψ <*> localize server ρ)
  --parallel servers \server un -> pure $ hash (viewFacet un server ) (viewFacet un server )
  α' <- gather servers servers α
  -- 3) Every server opens their commitments by publishing their ψ and ρ to each other
  ψ' <- gather servers servers ψ
  ρ' <- gather servers servers ρ
  -- 4) All servers verify each other's commitment by checking α = H(ρ, ψ)
  void $ enclave servers $ fanIn refl \server -> do
         ψ'' <- ψ'
         ρ'' <- ρ'
         α'' <- α'
         unless (α'' == (hash <$> ψ'' <*> ρ'')) $ locally_ server (liftIO $ throwIO CommitmentCheckFailed)
         pure $ pure ()
  -- 5) If all the checks are successful, then sum random values to get the random index.
  let ω = (\rho' -> sum rho' `mod` length (toLocs clients)) <$> ρ'
  chosenShares <- fanOut \server -> enclaveTo servers (server @@ nobody) (
                    ω >>= \omega -> enclave (server @@ nobody) do
                                                          ss <- localize server serverShares 
                                                          return $ toList ss !! omega
                    )
  -- Servers forward shares to an analyist.
  allShares <- gather servers (analyst @@ nobody) chosenShares
  locallyM_ analyst $ (putOutput "The answer is:" . sum) <$> allShares
  where
    hash :: Int -> Int -> Digest Crypto.SHA256
    hash ρ ψ = Crypto.hash $ toStrict (Binary.encode ρ <> Binary.encode ψ)

main :: IO ()
main = do
  [loc] <- getArgs
  let clientProof :: Subset '["client1", "client2"] '["client1", "client2", "server1", "server2", "analyst"]
      clientProof = explicitSubset
      serverProof :: Subset '["server1", "server2"] '["client1", "client2", "server1", "server2", "analyst"]
      serverProof = explicitSubset
      analystProof :: Member "analyst" '["client1", "client2", "server1", "server2", "analyst"]
      analystProof = explicitMember
  _ <- case loc of
    "client1" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "client1"
    "client2" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "client2"
    "server1" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "server1"
    "server2" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "server2"
    "analyst" -> runCLIIO $ runChoreography config (lottery clientProof serverProof analystProof) "analyst"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client1", ("localhost", 5000)),
          ("client2", ("localhost", 5001)),
          ("server1", ("localhost", 5002)),
          ("server2", ("localhost", 5003)),
          ("analyst", ("localhost", 5004))
        ]
