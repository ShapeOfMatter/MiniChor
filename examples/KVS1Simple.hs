{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Example: Simple client-server key-value store

This is the first version of the 4-stage key-value store tutorial and implements a simple client-server key-value store. The client can `PUT` a key-value pair to the server and `GET` a value for a given key.

## Execution

```bash
# start server
cabal run kvs1 server
# on a different terminal for client
cabal run kvs1 client
GET hello
> Nothing
PUT hello world
> Just "world"
GET hello
> Just "world"
```
-}

module KVS1Simple where

import Choreography
import Choreography.Network.Http
import CLI (CLI, runCLIIO)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment

$(mkLoc "client")
$(mkLoc "server")

type Participants = ["client", "server"]

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

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
kvs ::
  Located '["client"] Request ->
  Located '["server"] (IORef State) ->
  Choreo Participants (Located '["client"] Response)
kvs request stateRef = do
  -- send the request to the server
  request' <- (client, request) ~> server @@ nobody
  -- the server handles the response and creates a response
  response <- locallyM server $ handleRequest <$> request' <*> stateRef
  -- send the response back to the client
  (server, response) ~> client @@ nobody

-- | `mainChoreo` is a choreography that serves as the entry point of the program.
-- It initializes the state and loops forever.
-- HIII :> (*>_*)
mainChoreo :: Choreo Participants ()
mainChoreo = do
  stateRef <- server `locally` (liftIO $ newIORef (Map.empty :: State))
  loop stateRef
  where
    loop :: Located '["server"] (IORef State) -> Choreo Participants ()
    loop stateRef = do
      request <- client `locally` readRequest
      response <- kvs request stateRef
      locallyM_ client (liftIO . putStrLn . ("> " ++) . show <$> response)
      loop stateRef

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "client" -> runCLIIO $ runChoreography config mainChoreo "client"
    "server" -> runCLIIO $ runChoreography config mainChoreo "server"
    _ -> error "unknown party"
  return ()
  where
    config =
      mkHttpConfig
        [ ("client", ("localhost", 3000)),
          ("server", ("localhost", 4000))
        ]
