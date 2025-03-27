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
    EnclaveTo,
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
import CLI (CLI)
import Control.Monad (void)
import Data.List (delete)
import GHC.TypeLits

-- | A single value known to many parties.
newtype Located (ls :: [LocTy]) a = Located { naked :: forall census. Subset census ls -> Choreo census a }

notMine :: Located ls a
notMine = Located \_ -> pure undefined

data Choreo (ps :: [LocTy]) a where
  Locally ::
    (KnownSymbol l) =>
    CLI IO a ->
    Choreo '[l] a
  Broadcast ::
    (Show a, Read a, KnownSymbol l) =>
    Member l ps -> -- from
    (Member l ls, Located ls a) -> -- value
    Choreo ps a
  EnclaveTo ::
    (KnownSymbols inner) =>
    Subset inner outer ->
    Subset owners inner ->
    Choreo inner (Located owners b) ->
    Choreo outer (Located owners b)
  Return :: a -> Choreo ps a
  Bind :: Choreo ps a -> (a -> Choreo ps b) -> Choreo ps b

instance Functor (Choreo ps) where
  fmap f chor = Bind chor (Return . f) 

instance Applicative (Choreo ps) where
  pure = Return
  chorf <*> chora = Bind chorf (\f -> Bind chora (Return . f))

instance Monad (Choreo ps) where
  (>>=) = Bind

instance MonadFail (Choreo '[]) where
  fail _ = pure undefined -- Should be impossible-ish to call

instance (MonadFail (Choreo ps),
          KnownSymbol p,
          KnownSymbols ps) =>
          MonadFail (Choreo (p ': ps)) where
  fail message = do void . EnclaveTo (First @@ nobody) refl . Locally $ fail message
                    void . EnclaveTo (consSuper refl) refl $ fail message
                    pure undefined


-- | Run a `Choreo` monad with centralized semantics.
--   This basically pretends that the choreography is a single-threaded program and runs it all at once,
--   ignoring all the location aspects.
runChoreo :: forall census b p ps. (census ~ p ': ps) => Choreo census b -> CLI IO b
runChoreo = handler
  where
    handler :: Choreo census a -> CLI IO a
    handler (Locally m) = m
    handler (Broadcast _ (ownership, a)) = runChoreo $ naked a (ownership @@ nobody)
    handler (EnclaveTo (_ :: Subset ls (p ': ps)) _ c) = case tySpine @ls of
      TyNil -> pure notMine
      TyCons -> runChoreo c
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont

-- | Endpoint projection.
epp ::
  forall ps b.
  (KnownSymbols ps) =>
  -- | A choreography
  Choreo ps b ->
  -- | A `String` identifying a party.
  --   At present there is no enforcement that the party will actually be in the census of the choreography;
  --   some bugs may be possible if it is not.
  LocTm ->
  -- | Returns the implementation of the party's role in the choreography.
  Network (CLI IO) b
epp c l' = handler c
  where
    handler :: Choreo ps a -> Network (CLI IO) a
    handler (Locally m) = Run $ m
    handler (Broadcast s (l, a)) = do
      let sender = toLocTm s
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)
      if sender == l'
        then do val <- epp (naked a (l @@ nobody)) l'
                Send val otherRecipients
                pure val
        else Recv sender
    handler (EnclaveTo proof _ ch)
      | l' `elem` toLocs proof = epp ch l'
      | otherwise = pure notMine
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont
