{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Primary-backup key-value store

This is the second version of the 4-stage key-value store tutorial. This builds on the first version and adds a backup location to improve durability.

```bash
# start primary
cabal run kvs2 primary
# on a different terminal, start backup
cabal run kvs2 backup
# another terminal for client
cabal run kvs2 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS2PrimaryBackup where

import Choreography
import Choreography.Network.Http
import CLI (CLI, runCLIIO)
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

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs ::
  Located '["client"] Request ->
  (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) ->
  Choreo Participants (Located '["client"] Response)
kvs request (primaryStateRef, backupStateRef) = do
  -- send request to the primary node
  request' <- (client, request) ~> primary @@ nobody

  -- branch on the request
  broadcast First (primary @@ nobody) request' >>= \case
    -- if the request is a `PUT`, forward the request to the backup node
    Put _ _ -> do
      request'' <- (primary, request') ~> backup @@ nobody
      ack <- locallyM backup $ handleRequest <$> request'' <*> backupStateRef
      _ <- (backup, ack) ~> primary @@ nobody
      return ()
    _ -> do
      return ()

  -- process request on the primary node
  response <- locallyM primary $ handleRequest <$> request' <*> primaryStateRef

  -- send response to client
  (primary, response) ~> client @@ nobody

-- | `mainChoreo` is a choreography that serves as the entry point of the program.
-- It initializes the state and loops forever.
mainChoreo :: Choreo Participants ()
mainChoreo = do
  primaryStateRef <- primary `locally` liftIO ( newIORef (Map.empty :: State))
  backupStateRef <- backup `locally` liftIO (newIORef (Map.empty :: State))
  loop (primaryStateRef, backupStateRef)
  where
    loop :: (Located '["primary"] (IORef State), Located '["backup"] (IORef State)) -> Choreo Participants ()
    loop stateRefs = do
      request <- client `locally` readRequest
      response <- kvs request stateRefs
      locallyM_ client (liftIO . putStrLn . ("> " ++) . show <$> response)
      loop stateRefs

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runCLIIO $ runChoreography config mainChoreo "client"
    "primary" ->  runCLIIO $ runChoreography config mainChoreo "primary"
    "backup" -> runCLIIO $ runChoreography config mainChoreo "backup"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("primary", ("localhost", 4000)),
          ("backup", ("localhost", 5000))
        ]
