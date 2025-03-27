{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module ObliviousTransfer (ot2, ot4, main) where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Monad.IO.Class (MonadIO (liftIO))
-- For cryptonite

import Control.Monad (void)
import Crypto.Hash.Algorithms qualified as HASH
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.OAEP qualified as OAEP
import Crypto.Random.Types qualified as CRT
import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (ByteString, pack)
import GHC.TypeLits (KnownSymbol)
import System.Environment

-- Helpers for RSA encryption
genKeyPair :: (CRT.MonadRandom m) => m (RSA.PublicKey, RSA.PrivateKey)
genKeyPair = RSA.generate 64 65537

encryptRSA :: (CRT.MonadRandom m) => RSA.PublicKey -> Bool -> m ByteString
encryptRSA p a = do
  let bs = boolToByteString a
  x <- OAEP.encrypt (OAEP.defaultOAEPParams HASH.SHA1) p bs
  case x of
    Left _ -> undefined
    Right b -> return b

decryptRSA :: (CRT.MonadRandom m) => RSA.PrivateKey -> ByteString -> m Bool
decryptRSA r bs = do
  x <- OAEP.decryptSafer (OAEP.defaultOAEPParams HASH.SHA1) r bs
  case x of
    Left _ -> undefined
    Right b -> return $ byteStringToBool b

boolToByteString :: Bool -> BS.StrictByteString
boolToByteString = pack . show

byteStringToBool :: BS.StrictByteString -> Bool
byteStringToBool bs
  | bs == pack (show True) = True
  | bs == pack (show False) = False
  | otherwise = undefined

generateFakePK :: (CRT.MonadRandom m) => m RSA.PublicKey
generateFakePK = do
  bytes <- CRT.getRandomBytes 64
  return $ RSA.PublicKey 64 (bytesToInteger bytes) 65537
  where
    bytesToInteger bs = foldl (\acc byte -> (acc `shiftL` 8) + fromIntegral byte) 0 (BS.unpack bs)

--------------------------------------------------
-- 1-out-of-2 Oblivious transfer
--------------------------------------------------
ot2Insecure ::
  forall sender receiver m.
  (KnownSymbol sender, KnownSymbol receiver, MonadIO m) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot2Insecure b1 b2 s = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]
  sr <- (receiver, s) ~> sender @@ nobody
  message <- congruently3 (sender @@ nobody) (refl, sr) (refl, b1) (refl, b2) \sr' b1' b2' -> if sr' then b1' else b2'
  (sender, message) ~> receiver @@ nobody

genKeys :: (CRT.MonadRandom m) => Bool -> m (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey)
genKeys s = do
  -- Generate keys for OT. One key is real, and one is fake - select bit decides
  (pk, sk) <- genKeyPair
  fakePk <- generateFakePK
  return $ if s then (pk, fakePk, sk) else (fakePk, pk, sk)

encryptS ::
  (CRT.MonadRandom m) => -- Encryption based on select bit
  (RSA.PublicKey, RSA.PublicKey) ->
  Bool ->
  Bool ->
  m (ByteString, ByteString)
encryptS (pk1, pk2) b1 b2 = do c1 <- encryptRSA pk1 b1; c2 <- encryptRSA pk2 b2; return (c1, c2)

decryptS ::
  (CRT.MonadRandom m) => -- Decryption based on select bit
  (RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey) ->
  Bool ->
  (ByteString, ByteString) ->
  m Bool
decryptS (_, _, sk) s (c1, c2) = if s then decryptRSA sk c1 else decryptRSA sk c2

-- One out of two OT
ot2 :: forall sender receiver m.
  (KnownSymbol sender, KnownSymbol receiver, MonadIO m, CRT.MonadRandom m) =>
  Located '[sender] (Bool, Bool) ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot2 bb s = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  keys <- locally1 receiver (singleton, s) \s' -> liftIO $ genKeys s'
  pk1pk2 <- congruently1 (receiver @@ nobody) (refl, keys) \(pk1, pk2, _) -> (pk1, pk2)
  pks <- (receiver, pk1pk2) ~> sender @@ nobody
  encr <- locally2 sender (singleton, bb) (singleton, pks) \(b1, b2) pks' -> liftIO (encryptS pks' b1 b2)
  encrypted <- (sender, encr) ~> receiver @@ nobody
  locally3 receiver (singleton, keys) (singleton, s) (singleton, encrypted)
    \keys' s' encrypted' -> liftIO $ decryptS keys' s' encrypted'

--------------------------------------------------
-- 1-out-of-4 Oblivious transfer
--------------------------------------------------

select4 :: Bool -> Bool -> a -> a -> a -> a -> a
select4 s1 s2 v1 v2 v3 v4 = case (s1, s2) of
  (True, True) -> v1
  (True, False) -> v2
  (False, True) -> v3
  (False, False) -> v4

ot4Insecure ::
  forall sender receiver m.
  (KnownSymbol sender, KnownSymbol receiver, MonadIO m) =>
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[receiver] Bool -> -- receiver
  Located '[receiver] Bool -> -- receiver
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot4Insecure b1 b2 b3 b4 s1 s2 = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  s1r <- (receiver, s1) ~> sender @@ nobody
  s2r <- (receiver, s2) ~> sender @@ nobody
  b <- enclave (sender @@ nobody) do
    s1r' <- naked s1r refl
    s2r' <- naked s2r refl
    b1' <- naked b1 refl
    b2' <- naked b2 refl
    b3' <- naked b3 refl
    b4' <- naked b4 refl
    pure $ select4 s1r' s2r' b1' b2' b3' b4'
  (sender, b) ~> receiver @@ nobody

-- Generate keys for OT, only one has a SK and the rest are fake
genKeys4 ::
  (CRT.MonadRandom m) =>
  Bool ->
  Bool ->
  m (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey)
genKeys4 s1 s2 = do
  (pk, sk) <- genKeyPair
  fakePk1 <- generateFakePK
  fakePk2 <- generateFakePK
  fakePk3 <- generateFakePK
  return $ case (s1, s2) of
    (True, True) -> (pk, fakePk1, fakePk2, fakePk3, sk)
    (True, False) -> (fakePk1, pk, fakePk2, fakePk3, sk)
    (False, True) -> (fakePk1, fakePk2, pk, fakePk3, sk)
    (False, False) -> (fakePk1, fakePk2, fakePk3, pk, sk)

-- Encryption based on select bit
enc4 ::
  (CRT.MonadRandom m) =>
  (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey) ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  m (ByteString, ByteString, ByteString, ByteString)
enc4 (pk1, pk2, pk3, pk4) b1 b2 b3 b4 = do
  c1 <- encryptRSA pk1 b1
  c2 <- encryptRSA pk2 b2
  c3 <- encryptRSA pk3 b3
  c4 <- encryptRSA pk4 b4
  return (c1, c2, c3, c4)

-- Decryption based on select bit
dec4 ::
  (CRT.MonadRandom m) =>
  (RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PublicKey, RSA.PrivateKey) ->
  Bool ->
  Bool ->
  (ByteString, ByteString, ByteString, ByteString) ->
  m Bool
dec4 (_, _, _, _, sk) s1 s2 (c1, c2, c3, c4) = decryptRSA sk $ select4 s1 s2 c1 c2 c3 c4

-- One out of two OT
ot4 :: forall sender receiver m.
  (KnownSymbol sender, KnownSymbol receiver, MonadIO m, CRT.MonadRandom m) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (CLI m) (Located '[receiver] Bool)
ot4 b1 b2 b3 b4 s1 s2 = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  keys <- locally2 receiver (singleton, s1) (singleton, s2) \s1' s2' -> liftIO $ genKeys4 s1' s2'
  pk1234 <- congruently1 (receiver @@ nobody) (refl, keys) \(pk1, pk2, pk3, pk4, _) -> (pk1, pk2, pk3, pk4)
  pks <- (receiver, pk1234) ~> sender @@ nobody
  encr <- enclave (sender @@ nobody) do
    pks' <- naked pks refl
    b1' <- naked b1 refl
    b2' <- naked b2 refl
    b3' <- naked b3 refl
    b4' <- naked b4 refl
    locally' $ liftIO $ enc4 pks' b1' b2' b3' b4'
  encrypted <- (sender, encr) ~> receiver @@ nobody
  enclave (receiver @@ nobody) do
    keys' <- naked keys refl
    s1' <- naked s1 refl
    s2' <- naked s2 refl
    encrypted' <- naked encrypted refl
    locally' $ liftIO $ dec4 keys' s1' s2' encrypted'

-- Test function
otTest :: (KnownSymbol p1, KnownSymbol p2, MonadIO m, CRT.MonadRandom m) => Choreo '[p1, p2] (CLI m) ()
otTest = do
  let p1 = listedFirst :: Member p1 '[p1, p2]
  let p2 = listedSecond :: Member p2 '[p1, p2]
  bb <- p1 `locally` return (False, True)
  b1 <- congruently1 (p1 @@ nobody) (refl, bb) fst
  b2 <- congruently1 (p1 @@ nobody) (refl, bb) snd
  s <- p2 `locally` return False
  otResultI <- ot2Insecure b1 b2 s
  void $ locally1 p2 (singleton, otResultI) \res -> putOutput "OT2 insecure output:" res
  otResult <- ot2 bb s
  void $ locally1 p2 (singleton, otResult) \res -> putOutput "OT2 output:" res

  b3 <- p1 `locally` return False
  b4 <- p1 `locally` return True
  s2 <- p2 `locally` return False
  otResultI4 <- ot4Insecure b1 b2 b3 b4 s s2
  void $ locally1 p2 (singleton, otResultI4) \res -> putOutput "OT4 insecure output:" res
  otResult4 <- ot4 b1 b2 b3 b4 s s2
  void $ locally1 p2 (singleton, otResult4) \res -> putOutput "OT4 output:" res

main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "client1" -> runCLIIO $ runChoreography cfg (otTest @"client1" @"client2") "client1"
    "client2" -> runCLIIO $ runChoreography cfg (otTest @"client1" @"client2") "client2"
    _ -> error "unknown party"
  print delivery
  where
    cfg =
      mkHttpConfig
        [ ("client1", ("localhost", 4242)),
          ("client2", ("localhost", 4343))
        ]
