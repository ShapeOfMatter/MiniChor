-- | This module defines `Choreo`, the monad for writing choreographies,
--   and the closely related `Located` data type.
--   Not everything here is user-friendly; this is were we declare the foundational concepts.
--   These get repackaged in more convienent ways in "Choreography.Choreography"
--   and "Choreography.Choreography.Batteries".
module Choreography.Core
  ( -- * The `Choreo` monad and its operators
    Choreo(
    Locally,
    Broadcast,
    Enclave,
    Bind,
    Return),

    -- * Running choreographies
    epp,
    runChoreo,

    -- * Located values
    Located (Located),
    naked
  )
where

import Choreography.Locations
import Choreography.Network hiding (Bind, Return)
import Control.Monad (void)
import Data.List (delete)
import GHC.TypeLits

-- | A single value known to many parties.
newtype Located (ls :: [LocTy]) a = Located { naked :: forall census m. Subset census ls -> Choreo census m a }

notMine :: Located ls a
notMine = Located \_ -> pure undefined

data Choreo (ps :: [LocTy]) m a where
  Locally ::
    (KnownSymbol l) =>
    m a ->
    Choreo '[l] m a
  Broadcast ::
    (Show a, Read a, KnownSymbol l) =>
    Member l ps -> -- from
    (Member l ls, Located ls a) -> -- value
    Choreo ps m a
  Enclave ::
    (KnownSymbols ls) =>
    Subset ls ps ->
    Choreo ls m b ->
    Choreo ps m (Located ls b)
  Return :: a -> Choreo ps m a
  Bind :: Choreo ps m a -> (a -> Choreo ps m b) -> Choreo ps m b

instance Functor (Choreo ps m) where
  fmap f chor = Bind chor (Return . f) 

instance Applicative (Choreo ps m) where
  pure = Return
  chorf <*> chora = Bind chorf (\f -> Bind chora (Return . f))

instance Monad (Choreo ps m) where
  (>>=) = Bind

instance MonadFail (Choreo '[] m) where
  fail _ = pure undefined -- Should be impossible-ish to call

instance (MonadFail (Choreo ps m),
          KnownSymbol p,
          KnownSymbols ps,
          MonadFail m) =>
          MonadFail (Choreo (p ': ps) m) where
  fail message = do void . Enclave (First @@ nobody) . Locally $ fail message
                    void . Enclave (consSuper refl) $ fail message
                    pure undefined


-- | Run a `Choreo` monad with centralized semantics.
--   This basically pretends that the choreography is a single-threaded program and runs it all at once,
--   ignoring all the location aspects.
runChoreo :: forall census b m p ps. (Monad m, census ~ p ': ps) => Choreo census m b -> m b
runChoreo = handler
  where
    handler :: (Monad m) => Choreo census m a -> m a
    handler (Locally m) = m
    handler (Broadcast _ (ownership, a)) = runChoreo $ naked a (ownership @@ nobody)
    handler (Enclave (_ :: Subset ls (p ': ps)) c) = case tySpine @ls of
      TyNil -> pure notMine
      TyCons -> do v <- runChoreo c
                   pure $ Located \_ -> pure v
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont

-- | Endpoint projection.
epp ::
  forall ps b m.
  (Monad m, KnownSymbols ps) =>
  -- | A choreography
  Choreo ps m b ->
  -- | A `String` identifying a party.
  --   At present there is no enforcement that the party will actually be in the census of the choreography;
  --   some bugs may be possible if it is not.
  LocTm ->
  -- | Returns the implementation of the party's role in the choreography.
  Network m b
epp c l' = handler c
  where
    handler :: Choreo ps m a -> Network m a
    handler (Locally m) = Run $ m
    handler (Broadcast s (l, a)) = do
      let sender = toLocTm s
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)
      if sender == l'
        then do val <- epp (naked a (l @@ nobody)) l'
                Send val otherRecipients
                pure val
        else Recv sender
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = do val <- epp ch l'
                                    pure $ Located \_ -> pure val
      | otherwise = pure notMine
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont
