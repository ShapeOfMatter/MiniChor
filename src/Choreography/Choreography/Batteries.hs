-- | A zoo of helpful derived functions for writing choreographies.
module Choreography.Choreography.Batteries where

import Choreography.Choreography
import Choreography.Core
import Choreography.Locations
import CLI (CLI)
import Control.Monad (void)
import GHC.TypeLits

-- * Computation /per se/

-- | Perform a local computation, yielding nothing.
locally_ ::
  (KnownSymbol l) =>
  Member l ps ->
  CLI IO () ->
  Choreo ps ()

infix 4 `locally_`

locally_ l m = void $ locally l m

locally1 ::
  (KnownSymbol l) =>
  Member l census ->
  (Member l owners1, Located owners1 arg1) ->
  (arg1 -> CLI IO b) ->
  Choreo census (Located '[l] b)
locally1 present (owns1, a1) f =
    enclave (present @@ nobody) $ f
        <$> naked a1 (owns1 @@ nobody)
        >>= locally'

locally2 ::
  (KnownSymbol l) =>
  Member l census ->
  (Member l owners1, Located owners1 arg1) ->
  (Member l owners2, Located owners2 arg2) ->
  (arg1 -> arg2 -> CLI IO b) ->
  Choreo census (Located '[l] b)
locally2 present (owns1, a1) (owns2, a2) f =
    enclave (present @@ nobody) $ f
        <$> naked a1 (owns1 @@ nobody)
        <*> naked a2 (owns2 @@ nobody)
        >>= locally'

locally3 ::
  (KnownSymbol l) =>
  Member l census ->
  (Member l owners1, Located owners1 arg1) ->
  (Member l owners2, Located owners2 arg2) ->
  (Member l owners3, Located owners3 arg3) ->
  (arg1 -> arg2 -> arg3 -> CLI IO b) ->
  Choreo census (Located '[l] b)
locally3 present (owns1, a1) (owns2, a2) (owns3, a3) f =
    enclave (present @@ nobody) $ f
        <$> naked a1 (owns1 @@ nobody)
        <*> naked a2 (owns2 @@ nobody)
        <*> naked a3 (owns3 @@ nobody)
        >>= locally'

-- * Communication

-- | A variant of `~>` that sends the result of a local computation.
(~~>) ::
  forall a l ls' ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, CLI IO a) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps (Located ls' a)

infix 4 ~~>

(~~>) (l, m) ls' = do
  x <- locally l m
  (l, x) ~> ls'

-- | A variant of `~>` that sends the result of a local action that doesn't use existing `Located` variables.
(-~>) ::
  forall a l ls' ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, CLI IO a) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps (Located ls' a)

infix 4 -~>

(-~>) (l, m) ls' = do
  x <- l `locally` m
  (l, x) ~> ls'

-- * Enclaves

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond ::
  (KnownSymbols ls) =>
  -- | Tuple: Proof all the parties involved know the branch-guard
  -- and are present, the branch guard
  (Subset ls ps, (Subset ls qs, Located qs a)) ->
  -- | The body of the conditional as a function from the unwrapped value.
  (a -> Choreo ls b) ->
  Choreo ps (Located ls b)
cond (ls, (owns, a)) c = enclave ls $ naked a owns >>= c
