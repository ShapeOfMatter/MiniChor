-- | This module defines `Choreo`, the monad for writing choreographies,
--   and the closely related `Located` data type.
--   Not everything here is user-friendly; this is were we declare the foundational concepts.
--   These get repackaged in more convienent ways in "Choreography.Choreography"
--   and "Choreography.Choreography.Batteries".
module Choreography.Core
  ( -- * The `Choreo` monad and its operators
    Choreo(
    Locally,
    Congruently,
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
    othersForget,
    wrap, -- consider renaming or removing.
  )
where

import Choreography.Locations
import Choreography.Network hiding (Bind, Return)
import Control.Monad (void)
import Data.List (delete)
import GHC.TypeLits

-- | A single value known to many parties.
data Located (ls :: [LocTy]) a
  = Wrap a
  | Empty

-- | Wrap a value as a located value.
--   This should be safe to export, while exporting the constuctor would enable pattern matching.
wrap :: a -> Located l a
wrap = Wrap

-- | Unwraps values known to the specified party.
--   You should not be able to build such a function in normal code;
--   these functions are afforded only for use in "local" computation.
type Unwrap (q :: LocTy) = forall ls a. Member q ls -> Located ls a -> a

-- | Unwraps values known to the specified list of parties.
--   You should not be able to build such a function in normal code;
--   these functions are afforded only for use in "local" computation.
--   (Could be dangerous if the list is empty,
--   but the API is designed so that no value of type `Unwraps '[]` will ever actually get evaluated.)
type Unwraps (qs :: [LocTy]) = forall ls a. Subset qs ls -> Located ls a -> a

-- | Unwrap a `Located` value.
--   Unwrapping a empty located value will throw an exception; THIS SHOULD NOT BE EXPORTED!
unwrap :: Unwrap q
unwrap _ (Wrap a) = a
unwrap _ Empty = error "Located: This should never happen for a well-typed choreography."

-- | Cast a `Located` value to a smaller ownership set; useful when working with functions whos arguments have explict ownership sets.
othersForget :: Subset ls owners -> Located owners a -> Located ls a
othersForget _ Empty = Empty
othersForget _ (Wrap a) = Wrap a

data Choreo (ps :: [LocTy]) m a where
  Locally ::
    (KnownSymbol l) =>
    (Unwrap l -> m a) ->
    Choreo '[l] m a
  Congruently ::
    (KnownSymbols ls) =>
    (Unwraps ls -> a) ->
    Choreo ls m a
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
  fail message = do void . Enclave (First @@ nobody) . Locally $ \_ -> fail message
                    void . Enclave (consSuper refl) $ fail message
                    pure undefined


-- | Run a `Choreo` monad with centralized semantics.
--   This basically pretends that the choreography is a single-threaded program and runs it all at once,
--   ignoring all the location aspects.
runChoreo :: forall p ps b m. (Monad m) => Choreo (p ': ps) m b -> m b
runChoreo = handler
  where
    handler :: (Monad m) => Choreo (p ': ps) m a -> m a
    handler (Locally m) = m unwrap
    handler (Congruently f) =
      let unwraps :: forall c ls. Subset (p ': ps) ls -> Located ls c -> c
          unwraps = unwrap . (\(Subset mx) -> mx First) -- wish i could write this better.
       in pure . f $ unwraps
    handler (Broadcast _ (p, a)) = pure $ unwrap p a
    handler (Enclave (_ :: Subset ls (p ': ps)) c) = case tySpine @ls of
      TyNil -> pure Empty
      TyCons -> wrap <$> runChoreo c
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
    handler (Locally m) = Run $ m unwrap
    handler (Congruently f) =
      let unwraps :: forall c ls. Subset ps ls -> Located ls c -> c
          unwraps = case tySpine @ps of
            TyNil -> error "Undefined projection: the census is empty."
            TyCons -> unwrap . (\(Subset mx) -> mx First) -- wish i could write this better.
       in pure . f $ unwraps
    handler (Broadcast s (l, a)) = do
      let sender = toLocTm s
      let otherRecipients = sender `delete` toLocs (refl :: Subset ps ps)
      if sender == l'
        then do
          Send (unwrap l a) otherRecipients
          pure . unwrap l $ a
        else Recv sender
    handler (Enclave proof ch)
      | l' `elem` toLocs proof = wrap <$> epp ch l'
      | otherwise = pure Empty
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont
