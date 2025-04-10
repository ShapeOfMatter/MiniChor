-- | Operations for writing choreographies.
module Choreography.Choreography where

import Choreography.Core
import Choreography.Locations
import CLI (CLI)
import GHC.TypeLits

-- * Computation /per se/

-- | Access to the inner "local" monad.
--   Since the type of `locally'` restricts the census to a single party, you'll usually want to use
--   `Choreography.Choreography.locally` instead.
locally' ::
  (KnownSymbol l) =>
  -- | The local action(s)
  CLI IO a ->
  Choreo '[l] a
locally' m = Locally m

-- | Perform a local computation at a given location.
locally ::
  (KnownSymbol (l :: LocTy)) =>
  -- | Location performing the local computation.
  Member l ps ->
  -- | The local computation
  CLI IO a ->
  Choreo ps (Located '[l] a)

infix 4 `locally`

locally l m = enclave (l @@ nobody) $ locally' m

lMap :: 
  (KnownSymbols targets, KnownSymbols owners1) =>
  (a -> b) ->
  (Subset targets owners1, Subset owners1 census, Located owners1 a) ->
  Choreo census (Located targets b)
f `lMap` (owns1, presence1, la) = (f <$>) <$> othersForget owns1 presence1 la

lSplat ::
  (KnownSymbols targets, KnownSymbols owners2) =>
  Choreo census (Located targets (b -> c)) ->
  (Subset targets owners2, Subset owners2 census, Located owners2 b) ->
  Choreo census (Located targets c)
f `lSplat` (owns2, presence2, lb) = (<*>) <$> f <*> othersForget owns2 presence2 lb

{--- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently1 ::
  forall ls a census owners1 arg1 .
  (KnownSymbols ls) =>
  (Subset ls owners1, Subset owners1 census, Located owners1 arg1) ->
  (arg1 -> a) ->
  Choreo census (Located ls a)
congruently1 (owns1, presence1, arg1) f = do a1 <- othersForget owns1 presence1 arg1
                                             return $ f <$> a1

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently2 ::
  forall ls a census owners1 arg1 owners2 arg2 .
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (Subset ls owners2, Located owners2 arg2) ->
  (arg1 -> arg2 -> a) ->
  Choreo census (Located ls a)
congruently2 present (owns1, arg1) (owns2, arg2) f = enclave present $ f <$> naked arg1 owns1 <*> naked arg2 owns2

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently3 ::
  forall ls a census owners1 arg1 owners2 arg2 owners3 arg3 .
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (Subset ls owners2, Located owners2 arg2) ->
  (Subset ls owners3, Located owners3 arg3) ->
  (arg1 -> arg2 -> arg3 -> a) ->
  Choreo census (Located ls a)
congruently3 present (owns1, arg1) (owns2, arg2) (owns3, arg3) f = enclave present $ f <$> naked arg1 owns1 <*> naked arg2 owns2 <*> naked arg3 owns3


-- | Un-nest located values.
flatten :: (KnownSymbols ls)
        => Subset ls census
        -> Subset ls ms
        -> Subset ls ns
        -> Located ms (Located ns a)
        -> Choreo census (Located ls a)
flatten present ownsOuter ownsInner nested =
    enclave present do l <- naked nested ownsOuter
                       naked l ownsInner -}

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: (KnownSymbols ls, KnownSymbols owners)
             => Subset ls owners
             -> Subset owners census
             -> Located owners a
             -> Choreo census (Located ls a)
othersForget owns presence located = enclaveTo presence owns $ pure <$> located

-- * Communication

-- | Communicate a value to all present parties.
broadcast' ::
  (Show a, Read a, KnownSymbol l) =>
  -- | Proof the sender is present
  Member l ps ->
  -- | Proof the sender knows the value, the value.
  Located '[l] a ->
  Choreo ps a
broadcast' l a = Broadcast l a

-- | Send a value from one party to the entire census.
broadcast ::
  forall sender a owners census.
  (Show a, Read a, KnownSymbol sender, KnownSymbols census, KnownSymbols owners) =>
  Member sender owners ->
  Subset owners census ->
  Located owners a ->
  Choreo census a
broadcast owns present a = othersForget (owns @@ nobody) present a >>= broadcast' (inSuper present owns)

-- | Communication between a sender and a list of receivers.
(-~>) ::
  (Show a, Read a, KnownSymbol sender, KnownSymbols ls', KnownSymbols owners) =>
  (Member sender owners, Subset owners census, Located owners a) ->
  -- | The recipients.
  Subset ls' census ->
  Choreo census (Located ls' a)

infix 4 -~>

(owns, present, a) -~> rs = do
  x <- othersForget (owns @@ nobody) present a
  x' <- enclave (inSuper present owns @@ rs) $ broadcast' First x
  othersForget consSet (inSuper present owns @@ rs) x'

-- | Communication between a sender and a list of receivers.
(~>) ::
  (Show a, Read a, KnownSymbol sender, KnownSymbols ls') =>
  (Member sender census, Located '[sender] a) ->
  -- | The recipients.
  Subset ls' census ->
  Choreo census (Located ls' a)

infix 4 ~>

(present, a) ~> rs = do
  x' <- enclave (present @@ rs) $ broadcast' First a
  othersForget consSet (present @@ rs) x'

-- * Enclaves

-- | Lift a choreography of involving fewer parties into the larger party space.
enclaveTo ::
  (KnownSymbols ls) =>
  Subset ls ps ->
  Subset rs ls ->
  Choreo ls (Located rs a) ->
  Choreo ps (Located rs a)
enclaveTo proof1 proof2 ch = EnclaveTo proof1 proof2 ch

-- | Lift a choreography involving fewer parties into the larger party space.
enclaveToAll :: forall ls a ps . (KnownSymbols ls) => Subset ls ps -> Choreo ls (Located ls a) -> Choreo ps (Located ls a)

infix 4 `enclaveToAll`

enclaveToAll present ch = enclaveTo present refl ch

-- | Lift a choreography of involving fewer parties into the larger party space.
enclave ::
  forall ls a ps .
  (KnownSymbols ls) =>
  Subset ls ps ->
  Choreo ls a ->
  Choreo ps (Located ls a)

infix 4 `enclave`

enclave subcensus ch = enclaveToAll subcensus $ ch >>= pure . pure
