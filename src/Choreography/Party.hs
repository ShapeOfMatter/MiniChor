{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines locations (AKA parties)
--   and functions/relations pertaining to type-level lists of locations.
module Choreography.Party where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- * Type aliases

type PartyName = String

type PartyType = Symbol

newtype PartyID (l :: PartyType) = PartyID { asProxy :: Proxy l }

instance (KnownSymbol l) => Show (PartyID l) where
  show (PartyID prox) = symbolVal prox

-- * Membership and Subset proofs

class Member (p :: PartyType) (ps :: [PartyType])

instance {-# INCOHERENT #-} Member p (p ': ps)
instance {-# INCOHERENT #-} (Member p ps) => Member p (q ': ps)

class (forall p. Member p ps => Member p qs) => Subset ps qs
instance (forall p. Member p ps => Member p qs) => Subset ps qs

-- * Accessing parties' names

-- | Get the term-level list of names-as-strings for a proof-level list of parties.
namesOf :: forall (ls :: [PartyType]). (KnownSymbols ls) => PartiesSpine ls -> [PartyName]
namesOf _ = case partiesSpine @ls of -- this could be golfed by Quire, if that were defined here.
  PartiesNil -> []
  PartiesCons h ts -> show h : namesOf ts

-- * Handling type-level lists literals

-- $Handling
--
-- `KnownSymbols` constraints will often need to be declared in user code,
-- but using `partiesSpine` should only be necessary
-- when the behavior of the choreography depends on the structure of the type-level lists.
-- Most of the time the functions in "Choreography.Polymorphism" should do this for you.

-- | Term-level markers of the spine/structure of a type-level list.
--   Pattern matching on them recovers both the spine of the list and, if applicable,
--   `KnownSymbol`[@s@] instances for the head and tail.
data PartiesSpine ps where
  -- | Denotes that the list has a head and tail, and exposes `KnownSymbol` and `KnownSymbols` constraints respectively.
  PartiesCons :: (KnownSymbol h, KnownSymbols ts) => PartyID h -> PartiesSpine ts -> PartiesSpine (h ': ts)
  -- | Denotes that the list is empty.
  PartiesNil :: PartiesSpine '[]

-- | The type-level-list version of `GHC.TypeList.KnownSymbol`.
--   Denotes that both the spine of the list and each of its elements is known at compile-time.
--   This knowlege is typically recovered by recursively pattern-matching on @partiesSpine \@ls@.
class KnownSymbols ls where
  -- | Pattern matching on @partiesSpine \@ls@ will normally have two cases, for when @ls@ is empty or not.
  --   Contextual knowledge may let one or the other case be skipped.
  --   Within those cases, the knowledge afforded by `partiesSpine`'s constructors can be used.
  partiesSpine :: PartiesSpine ls

instance KnownSymbols '[] where
  partiesSpine = PartiesNil

instance (KnownSymbols ls, KnownSymbol l) => KnownSymbols (l ': ls) where
  partiesSpine = PartiesCons (PartyID Proxy) partiesSpine
