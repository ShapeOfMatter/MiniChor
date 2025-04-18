{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
-}

module KVS6SizePoly where

import CLI
import Choreography
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.TypeLits (KnownSymbol)
import Test.QuickCheck (Arbitrary, arbitrary, frequency, listOf)
import Text.Read (readMaybe)

readIORef :: (MonadIO m) => IORef a -> m a
readIORef = liftIO <$> IORef.readIORef

modifyIORef :: (MonadIO m) => IORef a -> (a -> a) -> m a
modifyIORef ref f = do
  a <- readIORef ref
  liftIO $ IORef.modifyIORef ref f
  return a

newIORef :: (MonadIO m) => a -> m (IORef a)
newIORef = liftIO <$> IORef.newIORef

-- $(mkLoc "client")

-- $(mkLoc "primary")

-- $(mkLoc "backup1")

-- $(mkLoc "backup2")
-- type Participants = ["client", "primary", "backup1", "backup2"]

kvs :: (KnownSymbol client) => ReplicationStrategy ps -> Member client ps -> Choreo ps ()
kvs ReplicationStrategy {setup, primary, handle} client = do
  rigging <- setup
  let go = do
        request <- (client, pure readRequest) ~~> primary @@ nobody
        response <- handle rigging request
        case response of
          Stopped -> return ()
          _ -> do
            client `locally_` putOutput "Recieved:" response
            go
  go

naryReplicationStrategy ::
  (KnownSymbol primary, KnownSymbols backups, KnownSymbols ps) =>
  Member primary ps ->
  Subset backups ps ->
  ReplicationStrategy ps
naryReplicationStrategy primary backups =
  ReplicationStrategy
    { primary,
      setup = servers `parallel` newIORef (Map.empty :: State),
      handle = \stateRef request -> do
        request' <- (primary, request) ~> servers
        localResponse <- fanOut \server -> othersForget (server @@ nobody) servers request' >>= \r ->
          enclave (inSuper servers server @@ nobody) do
            strf <- localize server stateRef
            r' <- r
            locally' $ handleRequest strf r'
        responses <- gather servers (primary @@ nobody) localResponse
        let response = ((\case
                        [r] -> r
                        rs -> Desynchronization rs
                       ) . nub . toList) <$> responses
        broadcast First (primary @@ nobody) response
    }
  where
    servers = primary @@ backups

data ReplicationStrategy ps
  = forall primary rigging.
  (KnownSymbol primary) =>
  ReplicationStrategy
  { primary :: Member primary ps,
    setup :: Choreo ps rigging,
    handle ::
      rigging ->
      Located '[primary] Request ->
      Choreo ps Response
  }

data Request = Put String String | Get String | Stop deriving (Eq, Ord, Read, Show)

data Response = Found String | NotFound | Stopped | Desynchronization [Response]
  deriving (Eq, Ord, Read, Show)

-- | PUT returns the old stored value; GET returns whatever was stored.
handleRequest :: (MonadIO m) => IORef State -> Request -> m Response
handleRequest stateRef (Put key value) = mlookup key <$> modifyIORef stateRef (Map.insert key value)
handleRequest stateRef (Get key) = mlookup key <$> readIORef stateRef
handleRequest _ Stop = return Stopped

mlookup :: String -> State -> Response
mlookup key = maybe NotFound Found . Map.lookup key

type State = Map String String

newtype Args = Args [Request] deriving (Eq, Ord, Read, Show)

instance Arbitrary Args where
  arbitrary = do
    reqs <- pgs
    return . Args $ reqs ++ [Stop]
    where
      pgs =
        listOf $
          frequency
            [ (1, Put <$> arbitrary <*> arbitrary),
              (1, Get <$> arbitrary)
            ]

readRequest :: CLI m Request
readRequest = do
  line <- getstr "Command?"
  case line of
    [] -> return Stop
    _ -> case readMaybe line of
      Just t -> return t
      Nothing -> putNote "Invalid command" >> readRequest

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy ::
  (KnownSymbol primary, KnownSymbols ps) =>
  Member primary ps ->
  ReplicationStrategy ps
nullReplicationStrategy primary =
  ReplicationStrategy
    { primary,
      setup = primary `locally` newIORef (Map.empty :: State),
      handle = \stateRef request -> do
        result <- locallyM primary $ handleRequest <$> stateRef <*> request
        broadcast First (primary @@ nobody) result
    }

naryHumans ::
  (KnownSymbol primary, KnownSymbols backups, KnownSymbols ps) =>
  Member primary ps ->
  Subset backups ps ->
  ReplicationStrategy ps
naryHumans primary backups =
  ReplicationStrategy
    { primary,
      setup = primary `locally` newIORef (Map.empty :: State),
      handle = \stateRef request -> do
        request' <- (primary, request) ~> backups
        backupResponse <- fanOut \server -> othersForget (server @@ nobody) backups request' >>= \r ->
          enclave (inSuper backups server @@ nobody) ( r >>= locally' . readResponse )
        localResponse <- locallyM primary $ handleRequest <$> stateRef <*> request
        responses <- gather backups (primary @@ nobody) backupResponse
        let response = (\lr rs -> case nub $ lr : toList rs of
                        [r] -> r
                        rs' -> Desynchronization rs'
                       ) <$> localResponse <*> responses
                      
        broadcast First (primary @@ nobody) response
    }
  where
    readResponse :: Request -> CLI m Response
    readResponse r = do
      line <- getstr $ show r ++ ": "
      case line of
        [] -> return NotFound
        _ -> return $ Found line

{-main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runChoreography config primaryBackupChoreo "client"
    "primary" -> runChoreography config primaryBackupChoreo "primary"
    "backup1" -> runChoreography config primaryBackupChoreo "backup1"
    "backup2" -> runChoreography config primaryBackupChoreo "backup2"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup1", ("localhost", 5000)),
          ("backup2", ("localhost", 6000))
        ]

-}
