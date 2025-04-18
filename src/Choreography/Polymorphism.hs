-- | Types, functions, and structures for writing choreographies with variable numbers of participants.
module Choreography.Polymorphism where

import Choreography.Choreography
import Choreography.Core
import Choreography.Locations
import CLI (CLI)
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Const (Const (Const, getConst))
import GHC.TypeLits

-- * The root abstraction

-- | A mapping, accessed by `Member` terms, from types (`Symbol`s) to values.
--   The types of the values depend on the indexing type; this relation is expressed by the type-level function @f@.
--   If the types of the values /don't/ depend on the index, use t`Quire`.
--   If the types vary only in that they are `Located` at the indexing party, use `Faceted`.
--   t`PIndexed` generalizes those two types in a way that's not usually necessary when writing choreographies.
newtype PIndexed ls f = PIndexed {pindex :: PIndex ls f}

-- | An impredicative quantified type. Wrapping it up in t`PIndexed` wherever possible will avoid a lot of type errors and headache.
type PIndex ls f = forall l. (KnownSymbol l) => Member l ls -> f l

-- | Sequence computations indexed by parties.
--   Converts a t`PIndexed` of computations into a computation yielding a t`PIndexed`.
--   Strongly analogous to 'Data.Traversable.sequence'.
--   In most cases, the [choreographic functions](#g:choreographicfunctions) below will be easier to use
--   than messing around with `Data.Functor.Compose.Compose`.
sequenceP ::
  forall b (ls :: [LocTy]) m.
  (KnownSymbols ls, Monad m) =>
  PIndexed ls (Compose m b) ->
  m (PIndexed ls b)
sequenceP (PIndexed f) = case tySpine @ls of
  TyCons -> do
    b <- getCompose $ f First
    PIndexed fTail <- sequenceP (PIndexed $ f . Later)
    let retVal :: PIndex ls b
        retVal First = b
        retVal (Later ltr) = fTail ltr
    pure $ PIndexed retVal
  TyNil -> pure $ PIndexed \case {}

-- * A type-indexed vector type

-- | A collection of values, all of the same type, assigned to each element of the type-level list.
newtype Quire parties a = Quire {asPIndexed :: PIndexed parties (Const a)}

-- | Access a value in a t`Quire` by its index.
getLeaf :: (KnownSymbol p) => Quire parties a -> Member p parties -> a
getLeaf (Quire (PIndexed q)) p = getConst $ q p

-- | Package a function as a t`Quire`.
stackLeaves :: forall ps a. (forall p. (KnownSymbol p) => Member p ps -> a) -> Quire ps a
stackLeaves f = Quire . PIndexed $ Const . f

-- | Get the head item from a t`Quire`.
qHead :: (KnownSymbol p) => Quire (p ': ps) a -> a
qHead (Quire (PIndexed f)) = getConst $ f First

-- | Get the tail of a t`Quire`.
qTail :: Quire (p ': ps) a -> Quire ps a
qTail (Quire (PIndexed f)) = Quire . PIndexed $ f . Later

-- | Prepend a value to a t`Quire`.
--   The corresponding `Symbol` to bind it to must be provided by type-application if it can't be infered.
qCons :: forall p ps a. a -> Quire ps a -> Quire (p ': ps) a
qCons a (Quire (PIndexed f)) = Quire . PIndexed $ \case
  First -> Const a
  Later mps -> f mps

-- | An empty t`Quire`.
qNil :: Quire '[] a
qNil = Quire $ PIndexed \case {}

-- | Apply a function to a single item in a t`Quire`.
qModify :: forall p ps a. (KnownSymbol p, KnownSymbols ps) => Member p ps -> (a -> a) -> Quire ps a -> Quire ps a
qModify First f q = f (qHead q) `qCons` qTail q
qModify (Later m) f q = case tySpine @ps of TyCons -> qHead q `qCons` qModify m f (qTail q)

instance forall parties. (KnownSymbols parties) => Functor (Quire parties) where
  fmap f q = case tySpine @parties of
    TyCons -> f (qHead q) `qCons` fmap f (qTail q)
    TyNil -> qNil

instance forall parties. (KnownSymbols parties) => Applicative (Quire parties) where
  pure a = Quire . PIndexed $ const (Const a)
  qf <*> qa = case tySpine @parties of
    TyCons -> qHead qf (qHead qa) `qCons` (qTail qf <*> qTail qa)
    TyNil -> qNil

instance forall parties. (KnownSymbols parties) => Foldable (Quire parties) where
  foldMap f q = case tySpine @parties of
    TyCons -> f (qHead q) <> foldMap f (qTail q)
    TyNil -> mempty

instance forall parties. (KnownSymbols parties) => Traversable (Quire parties) where
  sequenceA q = case tySpine @parties of
    TyCons -> qCons <$> qHead q <*> sequenceA (qTail q)
    TyNil -> pure qNil

instance forall parties a. (KnownSymbols parties, Eq a) => Eq (Quire parties a) where
  q1 == q2 = and $ (==) <$> q1 <*> q2

instance forall parties a. (KnownSymbols parties, Show a) => Show (Quire parties a) where
  show q = show $ toLocs (refl @parties) `zip` toList q

-- Many more instances are possible...

-- * Non-congruent parallel located values

-- | A unified representation of possibly-distinct homogeneous values owned by many parties.
type Faceted parties common a = PIndexed parties (Facet a common)

-- | Repackages `Located` with the type arguments correctly arranged for use with t`PIndexed`.
newtype Facet a common p = Facet {getFacet :: Located (p ': common) a}

-- | Get a `Located` value of a `Faceted` at a given location.
localize :: (KnownSymbol l) => Member l ls -> Faceted ls common a -> Located (l ': common) a
localize l (PIndexed f) = getFacet $ f l

{--- | In a context where unwrapping located values is possible, get the respective value stored in a `Faceted`.
viewFacet :: (KnownSymbol l, KnownSymbols qs) => Member l ls -> Subset qs (l ': common) -> Faceted ls common a -> Choreo qs a
viewFacet l qs f = naked (localize l f) qs-}

{-
unsafeFacet :: [Maybe a] -> Member l ls -> Facet a common l -- providing this as a helper function is pretty sketchy, if we don't need it delete it.
unsafeFacet (Just a : _) First = Facet $ wrap a
unsafeFacet (Nothing : _) First = Empty
unsafeFacet (_ : as) (Later l) = unsafeFacet as l
unsafeFacet [] _ = error "The provided list isn't long enough to use as a Faceted over the intended parties."
-}

-- * #choreographicfunctions# Choreographic functions

-- | Perform a local computation at all of a list of parties, yielding a `Faceted`.
parallel ::
  forall ls a ps .
  (KnownSymbols ls) =>
  -- | The parties who will do the computation must be present in the census.
  Subset ls ps ->
  -- | The local computation.
  CLI IO a ->
  Choreo ps (Faceted ls '[] a)
parallel ls m = parallel0 ls $ const m

-- | Perform a local computation, that doesn't use any existing `Located` values and doesn't depend on the respective party's identity,
--   at all of a list of parties, yielding a `Faceted`.
parallel_ ::
  forall ls ps .
  (KnownSymbols ls) =>
  Subset ls ps ->
  CLI IO () ->
  Choreo ps ()
parallel_ ls m = void $ parallel ls m

-- | Perform a local computation at all of a list of parties, yielding a `Faceted`.
parallel0 ::
  forall ls a ps .
  (KnownSymbols ls) =>
  -- | The parties who will do the computation must be present in the census.
  Subset ls ps ->
  -- | The local computation has access to the identity of the party in question.
  (forall l. (KnownSymbol l) => Member l ls -> CLI IO a) -> -- Could promote this to PIndexed too, but ergonomics might be worse?
  Choreo ps (Faceted ls '[] a)
parallel0 ls m = fanOut \mls -> locally (inSuper ls mls) (m mls)

-- | Perform a local computation at all of a list of parties, yielding nothing.
parallel0_ ::
  forall ls ps .
  (KnownSymbols ls) =>
  Subset ls ps ->
  (forall l. (KnownSymbol l) => Member l ls -> CLI IO ()) ->
  Choreo ps ()
parallel0_ ls m = void $ parallel0 ls m


-- | Perform a given choreography for each of several parties, giving each of them a return value that form a new `Faceted`.
fanOut ::
  (KnownSymbols qs) =>
  -- | The body.  -- kinda sketchy that rs might not be a subset of ps...
  (forall q. (KnownSymbol q) => Member q qs -> Choreo ps (Located (q ': rs) a)) ->
  Choreo ps (Faceted qs rs a)
fanOut body = sequenceP (PIndexed $ Compose . (Facet <$>) <$> body)

-- | Perform a given choreography for each of several parties; the return values are known to recipients but not necessarily to the loop-parties.
fanIn ::
  (KnownSymbols qs, KnownSymbols rs) =>
  -- | The recipients.
  Subset rs ps ->
  -- | The body.
  (forall q. (KnownSymbol q) => Member q qs -> Choreo ps (Located rs a)) ->
  Choreo ps (Located rs (Quire qs a))
fanIn rs body = do
  x <- Quire <$> sequenceP (PIndexed $ Compose . (Const <$>) <$> body)
  enclave rs $ sequence x
  --congruently1 rs \un -> stackLeaves $ \q -> un refl (getConst $ x q)

-- | The owner of a t`Quire` sends its elements to their respective parties, resulting in a `Faceted`.
--   This represents the "scatter" idea common in parallel computing contexts.
scatter ::
  forall census sender recipients a .
  (KnownSymbol sender, KnownSymbols recipients, Show a, Read a) =>
  Member sender census ->
  Subset recipients census ->
  Located '[sender] (Quire recipients a) ->
  Choreo census (Faceted recipients '[sender] a)
scatter sender recipients values = fanOut \r -> do
  message <- lMap (`getLeaf` r) (refl, sender @@ nobody, values)
  (sender, message) ~> inSuper recipients r @@ sender @@ nobody

-- | The owner of a t`Quire` sends its elements to their respective parties, resulting in a `Faceted`.
--   This represents the "scatter" idea common in parallel computing contexts.
scatter' ::
  forall census sender recipients a .
  (KnownSymbol sender, KnownSymbols recipients, Show a, Read a) =>
  Member sender census ->
  Subset recipients census ->
  Located '[sender] (Quire recipients a) ->
  Choreo census (Faceted recipients '[] a)
scatter' sender recipients values = scatter sender recipients values >>= cleanFacets recipients (sender @@ nobody)

-- | The many owners of a `Faceted` each send their respective values to a constant list of recipients, resulting in a t`Quire`.
--   This represents the "gather" idea common in parallel computing contexts.
gather ::
  forall census recipients senders a.
  (KnownSymbols senders, KnownSymbols recipients, Show a, Read a) =>
  Subset senders census ->
  Subset recipients census ->
  Faceted senders '[] a ->
  Choreo census (Located recipients (Quire senders a)) -- could be Faceted senders recipients instead...
gather senders recipients values = fanIn recipients \s ->
  (inSuper senders s, localize s values) ~> recipients

cleanFacets ::
  (KnownSymbols ps, KnownSymbols cruft) =>
  Subset ps census ->
  Subset cruft census ->
  Faceted ps cruft a ->
  Choreo census (Faceted ps '[] a)
cleanFacets ps cruft f = fanOut \p -> enclaveTo (inSuper ps p @@ cruft) (First @@ nobody) (pure <$> localize p f)
