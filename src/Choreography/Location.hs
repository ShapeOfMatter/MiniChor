{-# LANGUAGE DataKinds #-}

-- | This module defines locations and located values.
module Choreography.Location where

import Data.Proxy
import GHC.TypeLits
import Language.Haskell.TH

-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

-- | Convert a type-level location to a term-level location.
toLocTm :: forall (l :: LocTy). KnownSymbol l => Proxy l -> LocTm
toLocTm = symbolVal

-- | Located values.
--
-- @a \@ l@ represents a value of type @a@ at location @l@.
-- TODO type level sets of locations?
data a @ (l :: LocTy)
  = Wrap a -- ^ A located value @a \@ l@ from location @l@'s perspective.
  | Empty  -- ^ A located value @a \@ l@ from locations other than @l@'s
           -- perspective.

-- | Wrap a value as a located value.
wrap :: a -> a @ l
wrap = Wrap

-- | Unwrap a located value.
--
-- /Note:/ Unwrapping a empty located value will throw an exception.
unwrap :: a @ l-> a
unwrap (Wrap a) = a
unwrap Empty    = error "this should never happen for a well-typed choreography"

-- | Define a location at both type and term levels.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let p = mkName "Data.Proxy.Proxy"
  pure [SigD locName (AppT (ConT p) (LitT (StrTyLit loc))),ValD (VarP locName) (NormalB (ConE p)) []]


class Member x (xs :: [k]) where {}

instance {-# OVERLAPPABLE #-} (Member x xs) =>  Member x (y ': xs) where {}
instance {-# OVERLAPS #-} Member x (x ': xs) where {}

class SubSet xs ys where {}

instance {-# OVERLAPPABLE #-} (SubSet xs ys, Member x ys) => SubSet (x ': xs) ys where {}
instance {-# OVERLAPS #-} SubSet '[] ys where {}
