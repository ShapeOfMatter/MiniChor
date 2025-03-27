-- | This is just a wrapper module to allow more concise imports.
--     For documentation, you should probably start in "Choreography.Core".
module Choreography
  ( module Choreography.Core,
    module Choreography.Choreography,
    module Choreography.Choreography.Batteries,
    module Choreography.Locations,
    module Choreography.Locations.Batteries,
    module Choreography.Network,
    module Choreography.Polymorphism,

    -- * Running choreographies
    runChoreography,
  )
where

import Choreography.Choreography
import Choreography.Choreography.Batteries
import Choreography.Core hiding (Bind, Return)
import Choreography.Locations
import Choreography.Locations.Batteries
import Choreography.Network hiding (Bind, Return)
import Choreography.Polymorphism
import CLI (CLI)

-- | Run a choreography with a message transport backend...
runChoreography :: (Backend config, KnownSymbols ps) => config -> Choreo ps a -> LocTm -> CLI IO a
runChoreography cfg choreo l = runNetwork cfg l (epp choreo l)
