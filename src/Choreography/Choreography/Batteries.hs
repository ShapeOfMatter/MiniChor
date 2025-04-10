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

locallyM ::
  (KnownSymbol l) =>
  Member l census ->
  Located '[l] (CLI IO b) ->
  Choreo census (Located '[l] b)
locallyM present m = enclave (present @@ nobody) $ m >>= locally' 

locallyM_ ::
  (KnownSymbol l) =>
  Member l census ->
  Located '[l] (CLI IO ()) ->
  Choreo census ()
locallyM_ present m = void $ enclave (present @@ nobody) $ m >>= locally' 

-- * Communication

-- | A variant of `~>` that sends the result of a local computation.
(~~>) ::
  forall a l ls' ps.
  (Show a, Read a, KnownSymbol l, KnownSymbols ls') =>
  -- | A pair of a sender's location and a local computation.
  (Member l ps, Located '[l] (CLI IO a)) ->
  -- | A receiver's location.
  Subset ls' ps ->
  Choreo ps (Located ls' a)

infix 4 ~~>

(~~>) (l, m) ls' = do
  x <- locallyM l m
  (l, x) ~> ls'


-- * Enclaves

-- | Conditionally execute choreographies based on a located value. Automatically enclaves.
cond ::
  (KnownSymbols participants, KnownSymbols owners) =>
  -- | Tuple: Proof all the parties involved know the branch-guard
  -- and are present, the branch guard
  (Subset participants owners, Subset owners census, Located owners a) ->
  -- | The body of the conditional as a function from the unwrapped value.
  (a -> Choreo participants b) ->
  Choreo census (Located participants b)
cond (owns, present, a) c = do a' <- enclaveTo present owns (a >>= pure . pure)
                               enclave (transitive owns present) (a' >>= c)
