{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MPCFake where

import CLI
import Choreography
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data (TestArgs, reference)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import GHC.TypeLits (KnownSymbol)
import System.Random
import Test.QuickCheck (Arbitrary, arbitrary, chooseInt, elements, getSize, oneof, resize)

$(mkLoc "trusted3rdParty")
$(mkLoc "p1")
$(mkLoc "p2")
$(mkLoc "p3")
$(mkLoc "p4")

xor :: (Foldable f) => f Bool -> Bool
xor = foldr1 (/=)

data Circuit :: [LocTy] -> Type where
  InputWire :: (KnownSymbol p) => Member p ps -> Circuit ps
  LitWire :: Bool -> Circuit ps
  AndGate :: Circuit ps -> Circuit ps -> Circuit ps
  XorGate :: Circuit ps -> Circuit ps -> Circuit ps

instance Show (Circuit ps) where
  show (InputWire p) = "InputWire<" ++ toLocTm p ++ ">"
  show (LitWire b) = "LitWire " ++ show b
  show (AndGate left right) = "(" ++ show left ++ ") AND (" ++ show right ++ ")"
  show (XorGate left right) = "(" ++ show left ++ ") XOR (" ++ show right ++ ")"

instance Arbitrary (Circuit '["p1", "p2", "p3", "p4"]) where
  arbitrary = do
    size <- getSize
    if 1 >= size
      then oneof $ (LitWire <$> arbitrary) : (return <$> [InputWire p1, InputWire p2, InputWire p3, InputWire p4])
      else do
        left <- chooseInt (1, size)
        a <- resize left arbitrary
        b <- resize (1 `max` (size - left)) arbitrary
        op <- elements [AndGate, XorGate]
        return $ a `op` b

data Args = Args
  { circuit :: Circuit '["p1", "p2", "p3", "p4"],
    p1in :: Bool, -- These should be lists, but consuming them would be a chore...
    p2in :: Bool,
    p3in :: Bool,
    p4in :: Bool
  }
  deriving (Show)

instance Arbitrary Args where
  arbitrary = Args <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance TestArgs Args (Bool, Bool, Bool, Bool) where
  reference Args {circuit, p1in, p2in, p3in, p4in} = (answer, answer, answer, answer)
    where
      recurse c = case c of
        InputWire p -> fromJust $ toLocTm p `lookup` inputs
        LitWire b -> b
        AndGate left right -> recurse left && recurse right
        XorGate left right -> recurse left /= recurse right
      inputs = ["p1", "p2", "p3", "p4"] `zip` [p1in, p2in, p3in, p4in]
      answer = recurse circuit

secretShare ::
  forall p parties ps.
  (KnownSymbols parties, KnownSymbol p) =>
  Subset parties ps ->
  Member p ps ->
  Located '[p] Bool ->
  Choreo ps (Faceted parties '[] Bool)
secretShare parties p value = do
  shares <- locallyM p $ genShares <$> value
  scatter' p parties shares
  where
    genShares x = case tySpine @parties of
      TyCons -> gs'
      TyNil -> error "Can't secret-share to zero people."
      where
        gs' :: forall q qs. (KnownSymbol q, KnownSymbols qs) => CLI IO (Quire (q ': qs) Bool)
        gs' = do
          freeShares <- sequence $ pure $ liftIO randomIO -- generate n-1 random shares
          return $ xor (qCons @q x freeShares) `qCons` freeShares

reveal ::
  forall ps .
  (KnownSymbols ps) =>
  Faceted ps '[] Bool ->
  Choreo ps Bool
reveal shares = do
  let ps = allOf @ps
  allShares <- gather ps ps shares
  xor <$> allShares

computeWire ::
  (KnownSymbols ps, KnownSymbols parties, KnownSymbol trustedAnd) =>
  Member trustedAnd ps ->
  Subset parties ps ->
  Circuit parties ->
  Choreo ps (Faceted parties '[] Bool)
computeWire trustedAnd parties circuit = case circuit of
  InputWire p -> do
    value <- inSuper parties p `locally` getInput "Enter a secret input value:"
    secretShare parties (inSuper parties p) value
  LitWire b -> do
    let shares = partyNames `zip` (b : repeat False)
    fanOut \p -> inSuper parties p `locally` return (fromJust $ toLocTm p `lookup` shares)
  AndGate l r -> do
    lResult <- compute l
    rResult <- compute r
    inputShares <- fanIn (trustedAnd @@ nobody) \p -> do
      let results =  (,) <$> (localize p lResult) <*> (localize p rResult)
      (inSuper parties p, results) ~> trustedAnd @@ nobody
    let outputVal = (\ovs -> xor (fst <$> ovs) && xor (snd <$> ovs)) <$> inputShares
    secretShare parties trustedAnd outputVal
  XorGate l r -> do
    lResult <- compute l
    rResult <- compute r
    fanOut \p -> enclave (inSuper parties p @@ nobody) $ (/=) <$> localize p lResult <*> localize p rResult
  where
    compute = computeWire trustedAnd parties
    partyNames = toLocs parties

mpc ::
  (KnownSymbols parties) =>
  Circuit parties ->
  Choreo ("trusted3rdParty" ': parties) ()
mpc circuit = do
  let parties = consSuper refl
  outputWire <- computeWire trusted3rdParty parties circuit
  result <- enclave parties $ reveal outputWire
  void $ fanOut \p -> enclaveTo parties (p @@ nobody) $ result >>= (\r -> locally p (putOutput "The resulting bit:" r))
