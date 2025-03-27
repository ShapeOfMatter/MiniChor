{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Key-value store with location polymorphism

This is the final version of the 4-stage key-value store tutorial where we define the location-polymorphic choreography `doBackup`. We use it to define `doubleBackupReplicationStrategy`, which replicates data to two backup locations (`backup1` and `backup2`).

## Execution

```bash
# start primary
cabal run kvs4 primary
# on a different terminal, start backup1
cabal run kvs4 backup1
# another terminal for backup2
cabal run kvs4 backup2
# yet another terminal for client
cabal run kvs4 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS4LocPoly where

import Choreography
import Choreography.Network.Http
import CLI (CLI, runCLIIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.TypeLits (KnownSymbol)
import System.Environment

$(mkLoc "client")
$(mkLoc "primary")
$(mkLoc "backup1")
$(mkLoc "backup2")

type Participants = ["client", "primary", "backup1", "backup2"]

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

-- | `readRequest` reads a request from the terminal.
readRequest :: CLI IO Request
readRequest = do
  liftIO $ putStrLn "Command?"
  line <- liftIO $ getLine
  case parseRequest line of
    Just t -> return t
    Nothing -> liftIO (putStrLn "Invalid command") >> readRequest
  where
    parseRequest :: String -> Maybe Request
    parseRequest s =
      let l = words s
       in case l of
            ["GET", k] -> Just (Get k)
            ["PUT", k, v] -> Just (Put k v)
            _ -> Nothing

-- | `handleRequest` handle a request and returns the new the state.
handleRequest :: Request -> IORef State -> CLI IO Response
handleRequest request stateRef = case request of
  Put key value -> do
    liftIO $ modifyIORef stateRef (Map.insert key value)
    return (Just value)
  Get key -> do
    state <- liftIO $ readIORef stateRef
    return (Map.lookup key state)

-- | ReplicationStrategy specifies how a request should be handled on possibly replicated servers
-- `a` is a type that represent states across locations
type ReplicationStrategy a = Located '["primary"] Request -> a -> Choreo Participants (Located '["primary"] Response)

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State))
nullReplicationStrategy request stateRef = do
  locally2 primary (primary, request) (primary, stateRef) \req stRef -> case req of
    Put key value -> do
      liftIO $ modifyIORef stRef (Map.insert key value)
      return (Just value)
    Get key -> do
      state <- liftIO $ readIORef stRef
      return (Map.lookup key state)

-- | `doBackup` relays a mutating request to a backup location.
doBackup ::
  ( KnownSymbol a,
    KnownSymbol b,
    KnownSymbols ps
  ) =>
  Member a ps ->
  Member b ps ->
  Located '[a] Request ->
  Located '[b] (IORef State) ->
  Choreo ps ()
doBackup locA locB request stateRef = do
  broadcast (locA, request) >>= \case
    Put _ _ -> do
      request' <- (locA, request) ~> locB @@ nobody
      result <- locally2 locB (singleton, request') (singleton, stateRef) handleRequest
      _ <- (locB, result) ~> locA @@ nobody
      return ()
    _ -> do
      return ()

-- | `primaryBackupReplicationStrategy` is a replication strategy that replicates the state to a backup server.
primaryBackupReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State), Located '["backup1"] (IORef State))
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  doBackup primary backup1 request backupStateRef

  -- process request on primary
  locally2 primary (primary, request) (primary, primaryStateRef) handleRequest

-- | `doubleBackupReplicationStrategy` is a replication strategy that replicates the state to two backup servers.
doubleBackupReplicationStrategy ::
  ReplicationStrategy
    (Located '["primary"] (IORef State), Located '["backup1"] (IORef State), Located '["backup2"] (IORef State))
doubleBackupReplicationStrategy
  request
  (primaryStateRef, backup1StateRef, backup2StateRef) = do
    -- relay to two backup locations
    doBackup primary backup1 request backup1StateRef
    doBackup primary backup2 request backup2StateRef

    -- process request on primary
    locally2 primary (primary, request) (primary, primaryStateRef) handleRequest

-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs :: Located '["client"] Request -> a -> ReplicationStrategy a -> Choreo Participants (Located '["client"] Response)
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary @@ nobody

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client @@ nobody

-- | `nullReplicationChoreo` is a choreography that uses `nullReplicationStrategy`.
nullReplicationChoreo :: Choreo Participants ()
nullReplicationChoreo = do
  stateRef <- primary `locally` liftIO (newIORef (Map.empty :: State))
  loop stateRef
  where
    loop :: Located '["primary"] (IORef State) -> Choreo Participants ()
    loop stateRef = do
      request <- client `locally` readRequest
      response <- kvs request stateRef nullReplicationStrategy
      void $ locally1 client (client, response) (liftIO . print)
      loop stateRef

-- | `primaryBackupChoreo` is a choreography that uses `primaryBackupReplicationStrategy`.
primaryBackupChoreo :: Choreo Participants ()
primaryBackupChoreo = do
  primaryStateRef <- primary `locally` liftIO (newIORef (Map.empty :: State))
  backupStateRef <- backup1 `locally` liftIO (newIORef (Map.empty :: State))
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup1"] (IORef State)) -> Choreo Participants ()
    loop stateRefs = do
      request <- client `locally` readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      void $ locally1 client (client, response) (liftIO . print)
      loop stateRefs

-- | `doubleBackupChoreo` is a choreography that uses `doubleBackupReplicationStrategy`.
doubleBackupChoreo :: Choreo Participants ()
doubleBackupChoreo = do
  primaryStateRef <- primary `locally` liftIO (newIORef (Map.empty :: State))
  backup1StateRef <- backup1 `locally` liftIO (newIORef (Map.empty :: State))
  backup2StateRef <- backup2 `locally` liftIO (newIORef (Map.empty :: State))
  loop (primaryStateRef, backup1StateRef, backup2StateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup1"] (IORef State), Located '["backup2"] (IORef State)) -> Choreo Participants ()
    loop stateRefs = do
      request <- client `locally` readRequest
      response <- kvs request stateRefs doubleBackupReplicationStrategy
      void $ locally1 client (client, response) (liftIO . putStrLn . ("> " ++) . show)
      loop stateRefs

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runCLIIO $ runChoreography config primaryBackupChoreo "client"
    "primary" -> runCLIIO $ runChoreography config primaryBackupChoreo "primary"
    "backup1" -> runCLIIO $ runChoreography config primaryBackupChoreo "backup1"
    "backup2" -> runCLIIO $ runChoreography config primaryBackupChoreo "backup2"
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
