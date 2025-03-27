-- | This module defines the `Network` monad, which represents programs run on
-- individual nodes in a distributed system with explicit sends and receives.
-- To run a `Network` program, we provide a `runNetwork` function that supports
-- multiple message transport backends.
-- Two such backends are provided in "Choreography.Network.Http" and "Choreography.Network.Local",
-- and there should be enough tools here for you to write more as needed.
module Choreography.Network (
  Network(
  Run,
  Send,
  Recv,
  Bind,
  Return),
  Backend(runNetwork)
) where

import Choreography.Party (PartyName)
import Control.Monad.IO.Class

-- * The Network monad

-- | Effect signature for the `Network` monad.
data Network m a where
  -- | Local computation.
  Run ::
    m a ->
    Network m a
  -- | Sending.
  Send ::
    (Show a) =>
    a ->
    [PartyName] ->
    Network m ()
  -- | Receiving.
  Recv ::
    (Read a) =>
    PartyName ->
    Network m a
  Return :: a -> Network m a
  Bind :: Network m a -> (a -> Network m b) -> Network m b

instance Functor (Network m) where
  fmap f net = Bind net (Return . f ) 

instance Applicative (Network m) where
  pure = Return
  netf <*> neta = Bind netf (\f -> Bind neta (Return . f))

instance Monad (Network m) where
  (>>=) = Bind


-- * Message transport backends

-- | A message transport backend defines a /configuration/ of type @c@ that
-- carries necessary bookkeeping information, then defines @c@ as an instance
-- of `Backend` and provides a `runNetwork` function.
class Backend c where
  runNetwork :: (MonadIO m) => c -> PartyName -> Network m a -> m a
