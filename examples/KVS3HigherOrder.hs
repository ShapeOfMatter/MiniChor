{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Key-value store with higher-order choreography

## Execution

By default, `primaryBackupReplicationStrategy` will be used. Change `mainChoreo` to `nullReplicationChoreo` to use `nullReplicationStrategy`.

```bash
# start primary
cabal run kvs3 primary
# on a different terminal, start backup
cabal run kvs3 backup
# another terminal for client
cabal run kvs3 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS3HigherOrder where

import Choreography
import Choreography.Network.Http
import CLI (CLI, runCLIIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment

$(mkLoc "client")
$(mkLoc "primary")
$(mkLoc "backup")

type Participants = ["client", "primary", "backup"]

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

-- | `readRequest` reads a request from the terminal.
readRequest :: CLI IO Request
readRequest = do
  liftIO $ putStrLn "Command?"
  line <- liftIO getLine
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
type ReplicationStrategy a =
  Located '["primary"] Request -> a -> Choreo Participants (Located '["primary"] Response)

-- | `nullReplicationStrategy` is a replication strategy that does not replicate the state.
nullReplicationStrategy :: ReplicationStrategy (Located '["primary"] (IORef State))
nullReplicationStrategy request stateRef = locally2 primary (primary, request) (primary, stateRef) handleRequest

-- | `primaryBackupReplicationStrategy` is a replication strategy that replicates the state to a backup server.
primaryBackupReplicationStrategy ::
  ReplicationStrategy (Located '["primary"] (IORef State), Located '["backup"] (IORef State))
primaryBackupReplicationStrategy request (primaryStateRef, backupStateRef) = do
  -- relay request to backup if it is mutating (= PUT)
  broadcast (primary, request) >>= \case
    Put _ _ -> do
      request' <- (primary, request) ~> backup @@ nobody
      result <- locally2 backup (backup, request') (backup, backupStateRef) handleRequest
      _ <- (backup, result) ~> primary @@ nobody
      return ()
    _ -> do
      return ()

  -- process request on primary
  locally2 primary (primary, request) (primary, primaryStateRef) handleRequest

-- | `kvs` is a choreography that processes a single request at the client and returns the response.
-- It uses the provided replication strategy to handle the request.
kvs ::
  forall a.
  Located '["client"] Request ->
  a ->
  ReplicationStrategy a ->
  Choreo Participants (Located '["client"] Response)
kvs request stateRefs replicationStrategy = do
  request' <- (client, request) ~> primary @@ nobody

  -- call the provided replication strategy
  response <- replicationStrategy request' stateRefs

  -- send response to client
  (primary, response) ~> client @@ nobody

-- | `nullReplicationChoreo` is a choreography that uses `nullReplicationStrategy`.
nullReplicationChoreo :: Choreo Participants ()
nullReplicationChoreo = do
  stateRef <- primary `locally` liftIO ( newIORef (Map.empty :: State))
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
  backupStateRef <- backup `locally` liftIO (newIORef (Map.empty :: State))
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) -> Choreo Participants ()
    loop stateRefs = do
      request <- client `locally` readRequest
      response <- kvs request stateRefs primaryBackupReplicationStrategy
      void $ locally1 client (client, response) (liftIO . putStrLn . ("> " ++) . show)
      loop stateRefs

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runCLIIO $ runChoreography config mainChoreo "client"
    "primary" -> runCLIIO $ runChoreography config mainChoreo "primary"
    "backup" -> runCLIIO $ runChoreography config mainChoreo "backup"
    _ -> error "unknown party"
  return ()
  where
    mainChoreo = primaryBackupChoreo -- or `nullReplicationChoreo`
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup", ("localhost", 5000))
        ]
