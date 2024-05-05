{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type Located,
  KnownSymbols,
  mkLoc,
  Member,
  ExplicitMember,
  explicitMember,
  explicitSubset,
  inSuper,
  consSet,
  consSub,
  consSuper,
  singleton,
  nobody,
  (@@),
  flatten,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  locally_,
  _locally,
  _locally_,
  (~>),
  (~~>),
  cond,
  broadcastCond,
  cond',
  naked,
  enclave,
  fanOut,
  fanIn,

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
   mkHttpConfig,

  -- * Running choreographies
  runChoreo,
  runChoreography
  ) where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.IO.Class

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo ps m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
