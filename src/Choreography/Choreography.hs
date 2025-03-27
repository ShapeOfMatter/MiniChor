{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Operations for writing choreographies.
module Choreography.Choreography where

import Choreography.Core
import Choreography.Party
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits

-- * Computation /per se/

-- | Access to the inner "local" monad.
--   Since the type of `locally'` restricts the census to a single party, you'll usually want to use
--   `Choreography.Choreography.locally` instead.
locally' ::
  (KnownSymbol l) =>
  -- | The local action(s)
  m a ->
  Choreo '[l] m a
locally' m = Locally m

-- | Perform a local computation at a given location.
locally ::
  (KnownSymbol (l :: PartyType), Member l ps) =>
  -- | The local computation
  m a ->
  Choreo ps m (Located '[l] a)

infix 4 `locally`

locally m = enclave $ locally' m

{-
-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently1 ::
  forall ls a census owners1 arg1 m.
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (arg1 -> a) ->
  Choreo census m (Located ls a)
congruently1 present (owns1, arg1) f = enclave present $ f <$> naked owns1 arg1

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently2 ::
  forall ls a census owners1 arg1 owners2 arg2 m.
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (Subset ls owners2, Located owners2 arg2) ->
  (arg1 -> arg2 -> a) ->
  Choreo census m (Located ls a)
congruently2 present (owns1, arg1) (owns2, arg2) f = enclave present $ f <$> naked owns1 arg1 <*> naked owns2 arg2

-- | Perform the exact same pure computation in replicate at multiple locations.
--   The computation can not use anything local to an individual party, including their identity.
congruently3 ::
  forall ls a census owners1 arg1 owners2 arg2 owners3 arg3 m.
  (KnownSymbols ls) =>
  Subset ls census ->
  (Subset ls owners1, Located owners1 arg1) ->
  (Subset ls owners2, Located owners2 arg2) ->
  (Subset ls owners3, Located owners3 arg3) ->
  (arg1 -> arg2 -> arg3 -> a) ->
  Choreo census m (Located ls a)
congruently3 present (owns1, arg1) (owns2, arg2) (owns3, arg3) f = enclave present $ f <$> naked owns1 arg1 <*> naked owns2 arg2 <*> naked owns3 arg3
-}
-- | Unwrap a value known to the entire census.
naked ::
  (KnownSymbols ps, Subset ps qs) =>
  Located qs a ->
  Choreo ps m a
naked = Naked

-- | Un-nest located values.
flatten :: (KnownSymbols ls, Subset ls census, Subset ls ms, Subset ls ns)
        => Located ms (Located ns a)
        -> Choreo census m (Located ls a)
flatten nested =
    enclave $ naked nested >>= naked

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: (KnownSymbols ls, Subset ls census, Subset ls owners)
             => Located owners a
             -> Choreo census m (Located ls a)
othersForget located = enclave $ naked located


-- * Communication

-- | Send a value from one party to the entire census.
broadcast ::
  forall sender a census owners m.
  (Show a, Read a, KnownSymbol sender, KnownSymbols census, Member sender census, Member sender owners) =>
  PartyID sender ->
  Located owners a ->
  Choreo census m a
broadcast l a = Broadcast l a

comm ::
  forall recipients sender a census m.
  (Show a, Read a, KnownSymbol sender, KnownSymbols recipients,
   Member sender census, Subset recipients census) =>
  Located '[sender] a ->
  Choreo census m (Located recipients a)
comm l = do x <- enclave @(sender ': recipients) $ broadcast (PartyID (Proxy @sender)) l
            othersForget @recipients x

-- | Communication between a sender and a list of receivers.
commBy ::
  forall recipients sender a owners census m.
  (Show a, Read a, KnownSymbol sender, KnownSymbols recipients, Member sender census, Subset '[sender] owners, Subset recipients census) =>
  Located owners a ->
  Choreo census m (Located recipients a)
commBy l = do l' :: Located '[sender] a <- othersForget l
              undefined -- comm @recipients @sender l'

-- * Enclaves

-- | Lift a choreography of involving fewer parties into the larger party space.
--   Adds a `Located ls` layer to the return type.
enclave ::
  (KnownSymbols ls) =>
  Choreo ls m a ->
  Choreo ps m (Located ls a)
enclave ch = Enclave ch

-- | Lift a choreography of involving fewer parties into the larger party space.
--   This version, where the returned value is already Located, does not add a Located layer.
enclaveTo ::
  forall ls a rs ps m.
  (KnownSymbols ls,
   KnownSymbols rs) =>
  Choreo ls m (Located rs a) ->
  Choreo ps m (Located rs a)

infix 4 `enclaveTo`

enclaveTo ch = enclave ch >>= flatten
