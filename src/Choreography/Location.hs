{-# LANGUAGE DataKinds #-}

-- | This module defines locations and located values.
module Choreography.Location where

import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Language.Haskell.TH
import Logic.Proof (Proof, axiom)
import Logic.Classes (Reflexive, refl, Transitive, transitive)

-- | Term-level locations.
type LocTm = String

-- | Type-level locations.
type LocTy = Symbol

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


-- GDP has its own list logic, but IDK how to use it...
data IsMember (x :: k) (xs :: [k]) where {}
type Member x xs = Proof (IsMember x xs)
data IsSubset (xs :: [k]) (ys :: [k]) where {}
type Subset xs ys = Proof (IsSubset xs ys)
instance Reflexive IsSubset where {}
instance Transitive IsSubset where {}

class ExplicitMember (x :: k) (xs :: [k]) where
  explicitMember :: Member x xs
instance {-# OVERLAPPABLE #-} (ExplicitMember x xs) =>  ExplicitMember x (y ': xs) where
  explicitMember = inSuper consSet explicitMember
instance {-# OVERLAPS #-} ExplicitMember x (x ': xs) where
  explicitMember = axiom

consSet :: Subset xs (x ': xs)
consSet = consSuper refl  -- these are circular, is that bad?
consSuper :: forall xs ys y. Subset xs ys -> Subset xs (y ': ys)
consSuper sxy = transitive sxy consSet
consSub :: Subset xs ys -> Member x ys -> Subset (x ': xs) ys
consSub = const $ const axiom
inSuper :: Subset xs ys -> Member x xs -> Member x ys
inSuper _ _ = axiom

class ExplicitSubset xs ys where
  explicitSubset :: Subset xs ys

instance {-# OVERLAPPABLE #-} (ExplicitSubset xs ys, ExplicitMember x ys) => ExplicitSubset (x ': xs) ys where
  explicitSubset = consSub explicitSubset explicitMember
instance {-# OVERLAPS #-} ExplicitSubset '[] ys where
  explicitSubset = axiom


-- |Declare a proof-value with the given string as the variable name, proving that that string is a member of any list in which it explicitly apprears.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let tvar = mkName "xs"
  let m = mkName "Member"
  let eM = mkName "ExplicitMember"
  let em = mkName "explicitMember"
  pure [ SigD locName (ForallT [PlainTV tvar SpecifiedSpec]
                               [AppT (AppT (ConT eM) (LitT (StrTyLit loc))) (VarT tvar)]
                               (AppT (AppT (ConT m) (LitT (StrTyLit loc))) (VarT tvar)))
       , ValD (VarP locName) (NormalB (VarE em)) []
       ]

singleton :: forall p. (forall ps. (ExplicitMember p ps) => Member p ps) -> Member p (p ': '[])
singleton proof = proof  -- IKD why I can't just use id.

-- | Convert a type-level location to a term-level location.
toLocTm :: forall (l :: LocTy) (ps :: [LocTy]). KnownSymbol l => Member l ps -> LocTm
toLocTm _ = symbolVal (Proxy @l)
