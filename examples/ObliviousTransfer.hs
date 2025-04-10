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

import Crypto.Hash.Algorithms qualified as HASH
import Crypto.PubKey.RSA qualified as RSA
import Crypto.PubKey.RSA.OAEP qualified as OAEP
import Crypto.Random.Types qualified as CRT
import Data.Bits (shiftL)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (ByteString, pack)
import GHC.TypeLits (KnownSymbol)
import System.Environment

tupABof3 :: (a, b, c) -> (a, b)
tupABof3 (a, b, _) = (a, b)
tup4of5 :: (a, b, c, d, e) -> (a, b, c, d)
tup4of5 (a, b, c, d, _) = (a, b, c, d)

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
  forall sender receiver .
  (KnownSymbol sender, KnownSymbol receiver) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (Located '[receiver] Bool)
ot2Insecure b1 b2 s = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]
  sr <- (receiver, s) ~> sender @@ nobody
  let message = bool <$> sr <*> b1 <*> b2
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
ot2 :: forall sender receiver .
  (KnownSymbol sender, KnownSymbol receiver) =>
  Located '[sender] (Bool, Bool) ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (Located '[receiver] Bool)
ot2 bb s = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  keys <- locallyM receiver $ (liftIO . genKeys) <$> s
  let pk1pk2 = tupABof3 <$> keys
  pks <- (receiver, pk1pk2) ~> sender @@ nobody
  encr <- locallyM sender $ (\(b1, b2) pks' -> liftIO $ encryptS pks' b1 b2) <$> bb <*> pks
  encrypted <- (sender, encr) ~> receiver @@ nobody
  locallyM receiver $ (\keys' s' encrypted' -> liftIO $ decryptS keys' s' encrypted')
                      <$> keys <*> s <*> encrypted

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
  forall sender receiver .
  (KnownSymbol sender, KnownSymbol receiver) =>
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[sender] Bool -> -- sender
  Located '[receiver] Bool -> -- receiver
  Located '[receiver] Bool -> -- receiver
  Choreo '[sender, receiver] (Located '[receiver] Bool)
ot4Insecure b1 b2 b3 b4 s1 s2 = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  s1r <- (receiver, s1) ~> sender @@ nobody
  s2r <- (receiver, s2) ~> sender @@ nobody
  b <- enclave (sender @@ nobody) $
    select4 <$> s1r <*> s2r <*> b1 <*> b2 <*> b3 <*> b4
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
ot4 :: forall sender receiver .
  (KnownSymbol sender, KnownSymbol receiver) =>
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[sender] Bool ->
  Located '[receiver] Bool ->
  Located '[receiver] Bool ->
  Choreo '[sender, receiver] (Located '[receiver] Bool)
ot4 b1 b2 b3 b4 s1 s2 = do
  let sender = listedFirst :: Member sender '[sender, receiver]
  let receiver = listedSecond :: Member receiver '[sender, receiver]

  keys <- locallyM receiver $ (\s1' s2' -> liftIO $ genKeys4 s1' s2') <$> s1 <*> s2
  let pk1234 = tup4of5 <$> keys
  pks <- (receiver, pk1234) ~> sender @@ nobody
  encr <- enclave (sender @@ nobody) do
    pks' <- pks
    b1' <- b1
    b2' <- b2
    b3' <- b3
    b4' <- b4
    locally' $ liftIO $ enc4 pks' b1' b2' b3' b4'
  encrypted <- (sender, encr) ~> receiver @@ nobody
  enclave (receiver @@ nobody) do
    keys' <- keys
    s1' <- s1
    s2' <- s2
    encrypted' <- encrypted
    locally' $ liftIO $ dec4 keys' s1' s2' encrypted'

-- Test function
otTest :: (KnownSymbol p1, KnownSymbol p2) => Choreo '[p1, p2] ()
otTest = do
  let p1 = listedFirst :: Member p1 '[p1, p2]
  let p2 = listedSecond :: Member p2 '[p1, p2]
  bb <- p1 `locally` return (False, True)
  let b1 = fst <$> bb
      b2 = snd <$> bb
  s <- p2 `locally` return False
  otResultI <- ot2Insecure b1 b2 s
  locallyM_ p2 $ (putOutput "OT2 insecure output:") <$> otResultI
  otResult <- ot2 bb s
  locallyM_ p2 $ (putOutput "OT2 output:") <$> otResult

  b3 <- p1 `locally` return False
  b4 <- p1 `locally` return True
  s2 <- p2 `locally` return False
  otResultI4 <- ot4Insecure b1 b2 b3 b4 s s2
  locallyM_ p2 $ (putOutput "OT4 insecure output:") <$> otResultI4
  otResult4 <- ot4 b1 b2 b3 b4 s s2
  locallyM_ p2 $ (putOutput "OT4 output:") <$> otResult4

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
