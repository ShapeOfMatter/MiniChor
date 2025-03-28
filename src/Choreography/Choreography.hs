-- | Operations for writing choreographies.
module Choreography.Choreography where

import Choreography.Core
import Choreography.Locations
import Choreography.Locations.Batteries (ExplicitMember (..))
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

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently1 ::
  forall ls a census owners1 arg1 .
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (arg1 -> a) ->
  Choreo census (Located ls a)
congruently1 present (owns1, arg1) f = enclave present $ f <$> naked arg1 owns1

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
                       naked l ownsInner

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: (KnownSymbols ls)
             => Subset ls census
             -> Subset ls owners
             -> Located owners a
             -> Choreo census (Located ls a)
othersForget present owns located = enclave present $ naked located owns


-- * Communication

-- | Writing out the first argument to `~>` can be done a few different ways depending on context, represented by this class.
class (KnownSymbol loc) => CanSend struct loc val owners census | struct -> loc val owners census where
  presentToSend :: struct -> Member loc census
  ownsMessagePayload :: struct -> Member loc owners
  structMessagePayload :: struct -> Located owners val

instance (KnownSymbol l) => CanSend (Member l ps, (Member l ls, Located ls a)) l a ls ps where
  presentToSend = fst
  ownsMessagePayload = fst . snd
  structMessagePayload = snd . snd

instance (KnownSymbol l, ExplicitMember l ls) => CanSend (Member l ps, Located ls a) l a ls ps where
  presentToSend = fst
  ownsMessagePayload = const explicitMember
  structMessagePayload = snd

instance (KnownSymbol l) => CanSend (Member l ls, Subset ls ps, Located ls a) l a ls ps where
  presentToSend (m, s, _) = inSuper s m
  ownsMessagePayload (m, _, _) = m
  structMessagePayload (_, _, p) = p

-- | Communicate a value to all present parties.
broadcast' ::
  (Show a, Read a, KnownSymbol l) =>
  -- | Proof the sender is present
  Member l ps ->
  -- | Proof the sender knows the value, the value.
  (Member l ls, Located ls a) ->
  Choreo ps a
broadcast' l a = Broadcast l a

-- | Send a value from one party to the entire census.
broadcast ::
  forall l a ps ls s.
  (Show a, Read a, KnownSymbol l, KnownSymbols ps, CanSend s l a ls ps) =>
  s ->
  Choreo ps a
broadcast s = broadcast' (presentToSend s) (ownsMessagePayload s, structMessagePayload s)

-- | Communication between a sender and a list of receivers.
(~>) ::
  (Show a, Read a, KnownSymbol l, KnownSymbols ls', CanSend s l a ls ps) =>
  -- | The message argument can take three forms:
  --
  --   >  (Member sender census, wrapped owners a) -- where sender is explicitly listed in owners
  --
  --   >  (Member sender owners, Subset owners census, wrapped owners a)
  --
  --   >  (Member sender census, (Member sender owners, wrapped owners a)
  s ->
  -- | The recipients.
  Subset ls' ps ->
  Choreo ps (Located ls' a)

infix 4 ~>

s ~> rs = do
  x <- enclave (presentToSend s @@ rs) $ broadcast' First (ownsMessagePayload s, structMessagePayload s)
  othersForget rs consSet x

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

enclave subcensus ch = enclaveToAll subcensus do x <- ch
                                                 pure $ Located \_ -> pure x
