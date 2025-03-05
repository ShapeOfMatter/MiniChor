-- | Operations for writing choreographies.
module Choreography.Choreography where

import Choreography.Core
import Choreography.Locations
import Choreography.Locations.Batteries (ExplicitMember (..))
import GHC.TypeLits

-- * Computation /per se/

-- | Access to the inner "local" monad.
--   Since the type of `locally'` restricts the census to a single party, you'll usually want to use
--   `Choreography.Choreography.locally` instead.
locally' ::
  (KnownSymbol l) =>
  -- | The local action(s), which can use an unwraper function.
  (Unwrap l -> m a) ->
  Choreo '[l] m a
locally' m = Locally m

-- | Perform a local computation at a given location.
locally ::
  (KnownSymbol (l :: LocTy)) =>
  -- | Location performing the local computation.
  Member l ps ->
  -- | The local computation, which can use a constrained unwrap function.
  (Unwrap l -> m a) ->
  Choreo ps m (Located '[l] a)

infix 4 `locally`

locally l m = enclave (l @@ nobody) $ locally' m

-- | Perform the exact same computation in replicate at all participating locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently' ::
  (KnownSymbols ls) =>
  -- | The computation, which can use an unwraper function.
  (Unwraps ls -> a) ->
  Choreo ls m a
congruently' f = Congruently f

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently ::
  forall ls a ps m.
  (KnownSymbols ls) =>
  Subset ls ps ->
  (Unwraps ls -> a) ->
  Choreo ps m (Located ls a)

infix 4 `congruently`

congruently ls a = enclave ls $ congruently' a

-- | Unwrap a value known to the entire census.
naked ::
  (KnownSymbols ps) =>
  Subset ps qs ->
  Located qs a ->
  Choreo ps m a
naked ownership a = congruently' (\un -> un ownership a)

-- | Un-nest located values.
flatten :: (KnownSymbols ls)
        => Subset ls census
        -> Subset ls ms
        -> Subset ls ns
        -> Located ms (Located ns a)
        -> Choreo census m (Located ls a)
flatten present ownsOuter ownsInner nested =
    enclave present $ naked ownsOuter nested >>= naked ownsInner

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: (KnownSymbols ls)
             => Subset ls census
             -> Subset ls owners
             -> Located owners a
             -> Choreo census m (Located ls a)
othersForget present owns located = enclave present $ naked owns located


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
  Choreo ps m a
broadcast' l a = Broadcast l a

-- | Send a value from one party to the entire census.
broadcast ::
  forall l a ps ls m s.
  (Show a, Read a, KnownSymbol l, KnownSymbols ps, CanSend s l a ls ps) =>
  s ->
  Choreo ps m a
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
  Choreo ps m (Located ls' a)

infix 4 ~>

s ~> rs = do
  x :: a <- enclave (presentToSend s @@ rs) $ broadcast' First (ownsMessagePayload s, structMessagePayload s)
  congruently rs (\un -> un consSet x)

-- * Enclaves

-- | Lift a choreography of involving fewer parties into the larger party space.
--   Adds a `Located ls` layer to the return type.
enclave ::
  (KnownSymbols ls) =>
  Subset ls ps ->
  Choreo ls m a ->
  Choreo ps m (Located ls a)
enclave proof ch = Enclave proof ch

-- | Lift a choreography involving fewer parties into the larger party space.
--   This version, where the returned value is Located at the entire enclave, does not add a Located layer.
enclaveToAll :: forall ls a ps m. (KnownSymbols ls) => Subset ls ps -> Choreo ls m (Located ls a) -> Choreo ps m (Located ls a)

infix 4 `enclaveToAll`

enclaveToAll = (`enclaveTo` (refl @ls))

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is already Located, does not add a Located layer.
enclaveTo ::
  forall ls a rs ps m.
  (KnownSymbols ls,
   KnownSymbols rs) =>
  Subset ls ps ->
  Subset rs ls ->
  Choreo ls m (Located rs a) ->
  Choreo ps m (Located rs a)

infix 4 `enclaveTo`

enclaveTo subcensus recipients ch = do nested <- subcensus `enclave` ch
                                       flatten (recipients `transitive` subcensus) recipients (refl @rs) nested
