-- | Additional functions/relations pertaining to locations and type-level lists of locations.
module Choreography.Locations.Batteries where

import Choreography.Party
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH

-- * Misc

-- * Context manipulation

{--- | Use any membership proof to to safely call code that only works on a non-empy list.
quorum1 ::
  forall ps p a.
  (KnownSymbols ps) =>
  Member p ps ->
  (forall q qs. (KnownSymbol q, KnownSymbols qs, ps ~ q ': qs) => a) ->
  a
quorum1 p a = case (p, tySpine @ps) of
  (First, TyCons) -> a
  (Later _, TyCons) -> a

-- * Template Haskell

-- | Declare a proof-value with the given string as the variable name, proving that that string is a member of any list in which it explicitly apprears.
mkLoc :: String -> Q [Dec]
mkLoc loc = do
  let locName = mkName loc
  let tvar = mkName "xs"
  let m = mkName "Member"
  let eM = mkName "ExplicitMember"
  let em = mkName "explicitMember"
  pure
    [ SigD
        locName
        ( ForallT
            [PlainTV tvar SpecifiedSpec]
            [AppT (AppT (ConT eM) (LitT (StrTyLit loc))) (VarT tvar)]
            (AppT (AppT (ConT m) (LitT (StrTyLit loc))) (VarT tvar))
        ),
      ValD (VarP locName) (NormalB (VarE em)) []
    ]-}
