{-# LANGUAGE UndecidableInstances #-}
-- | This module defines `Choreo`, the monad for writing choreographies,
--   and the closely related `Located` data type.
--   Not everything here is user-friendly; this is were we declare the foundational concepts.
--   These get repackaged in more convienent ways in "Choreography.Choreography"
--   and "Choreography.Choreography.Batteries".
module Choreography.Core
  ( -- * The `Choreo` monad and its operators
    Choreo(
    Locally,
    Naked,
    Broadcast,
    Enclave,
    Bind,
    Return),

    -- * Running choreographies
    epp,
    runChoreo,

    -- * Located values
    Located (),
    Unwrap,
    Unwraps,
    wrap, -- consider renaming or removing.
  )
where

import Choreography.Party
import Choreography.Network hiding (Bind, Return)
import Control.Monad (void)
import Data.List (delete)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits

-- | A single value known to many parties.
data Located (ls :: [PartyType]) a
  = Wrap a
  | Empty

-- | Wrap a value as a located value.
--   This should be safe to export, while exporting the constuctor would enable pattern matching.
wrap :: a -> Located l a
wrap = Wrap

-- | Unwraps values known to the specified party.
--   You should not be able to build such a function in normal code;
--   these functions are afforded only for use in "local" computation.
type Unwrap (q :: PartyType) = forall ls a. Member q ls => Located ls a -> a

-- | Unwraps values known to the specified list of parties.
--   You should not be able to build such a function in normal code;
--   these functions are afforded only for use in "local" computation.
--   (Could be dangerous if the list is empty,
--   but the API is designed so that no value of type `Unwraps '[]` will ever actually get evaluated.)
type Unwraps (qs :: [PartyType]) = forall ls a. Subset qs ls => Located ls a -> a

-- | Unwrap a `Located` value.
--   Unwrapping a empty located value will throw an exception; THIS SHOULD NOT BE EXPORTED!
unwrap :: forall self. forall owners a. Member self owners => PartyID self -> Located owners a -> a
unwrap _ (Wrap a) = a
unwrap _ Empty = error "Located: This should never happen for a well-typed choreography."

data Choreo (ps :: [PartyType]) m a where
  Locally ::
    (KnownSymbol l) =>
    m a ->
    Choreo '[l] m a
  Naked :: forall census owners a m.
    Subset census owners =>
    Located owners a ->
    Choreo census m a
  Broadcast :: forall sender census owners a m.
    (Show a, Read a, KnownSymbol sender, Member sender census, Member sender owners) =>
    PartyID sender ->
    Located owners a -> -- value
    Choreo census m a
  Enclave :: forall inner outer a m.
    (KnownSymbols inner, Subset inner outer) =>
    Choreo inner m a ->
    Choreo outer m (Located inner a)
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
          Subset '[p] (p ': ps),
          Subset ps (p ': ps),
          MonadFail m) =>
         MonadFail (Choreo (p ': ps) m) where
  fail message = do void . Enclave @'[p] @(p ': ps) . Locally $ fail @m message
                    void . Enclave $ fail @(Choreo ps m) message
                    pure undefined


-- | Run a `Choreo` monad with centralized semantics.
--   This basically pretends that the choreography is a single-threaded program and runs it all at once,
--   ignoring all the location aspects.
runChoreo :: forall census b m p ps. (Monad m, census ~ p ': ps) => Choreo census m b -> m b
runChoreo = handler
  where
    handler :: (Monad m) => Choreo census m a -> m a
    handler (Locally m) = m
    handler (Naked a) = pure $ unwrap (PartyID (Proxy @p)) a
    handler (Broadcast s a) = pure $ unwrap s a
    handler (Enclave (c :: Choreo ls m a)) = case partiesSpine @ls of
      PartiesNil -> pure Empty
      PartiesCons _ _ -> wrap <$> runChoreo c
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
  PartyName ->
  -- | Returns the implementation of the party's role in the choreography.
  Network m b
epp c l' = handler c
  where
    handler :: Choreo ps m a -> Network m a
    handler (Locally m) = Run $ m
    handler (Naked a) =
      let unwraps :: forall c ls. Subset ps ls => Located ls c -> c
          unwraps = case partiesSpine @ps of
            PartiesNil -> error "Undefined projection: the census is empty."
            PartiesCons h _ -> unwrap h
       in pure $ unwraps a
    handler (Broadcast s a) = do
      let sender = show s
      let otherRecipients = sender `delete` namesOf (partiesSpine @ps)
      if sender == l'
        then do
          Send (unwrap s a) otherRecipients
          pure . unwrap s $ a
        else Recv sender
    handler (Enclave (ch :: Choreo inner m a))
      | l' `elem` namesOf (partiesSpine @inner) = wrap <$> epp ch l'
      | otherwise = pure Empty
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont
