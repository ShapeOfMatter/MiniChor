{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KVS7SimplePoly where

-- # This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

import Choreography
import CLI (CLI, runCLIIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef)

type Response = Int

type Key = String

type State = String

errorResponse :: Response
errorResponse = -1

data Request
  = Get Key
  | Put Key Int
  deriving (Eq, Read, Show)

handleGet :: IORef State -> Key -> CLI IO Response
handleGet s k = do
  liftIO (readIORef s) >>= (liftIO . putStrLn)
  return $ length k

handlePut :: IORef State -> Key -> Int -> CLI IO Response
handlePut s k v = do
  liftIO (readIORef s) >>= (liftIO . putStrLn)
  return . fromEnum $ v /= length k

isOk :: Response -> Bool
isOk = (== 0)

handleRequest ::
  forall backups.
  (KnownSymbols backups) =>
  Located '["primary"] Request ->
  (Located '["primary"] (IORef State), Faceted backups '[] (IORef State)) ->
  Choreo ("primary" ': backups) (Located '["primary"] Response)
handleRequest request (primaryStateRef, backupsStateRefs) =
  broadcast First (primary @@ nobody) request >>= \case
    Put key value -> do
      oks <- fanOut \backup -> enclave (inSuper backups backup @@ nobody) do
               bsr <- localize backup backupsStateRefs
               locally' $ handlePut bsr key value
      gathered <- gather backups (primary @@ nobody) oks
      locallyM primary $ (\gathered' stRef ->
        if all isOk gathered'
          then handlePut stRef key value
          else return errorResponse
        ) <$> gathered <*> primaryStateRef
    Get key -> locallyM primary $ (`handleGet` key) <$> primaryStateRef
  where
    primary :: forall ps. Member "primary" ("primary" ': ps)
    primary = listedFirst
    backups = consSuper refl

kvs ::
  forall backups.
  (KnownSymbols backups) =>
  Located '["client"] Request ->
  (Located '["primary"] (IORef State), Faceted backups '[] (IORef State)) ->
  Choreo ("client" ': "primary" ': backups) (Located '["client"] Response)
kvs request stateRefs = do
  request' <- (client, request) ~> primary @@ nobody
  response <- enclaveTo (primary @@ backups) (First @@ nobody) (handleRequest request' stateRefs)
  (primary, response) ~> client @@ nobody
  where
    client :: forall ps. Member "client" ("client" ': ps)
    primary :: forall ps p. Member "primary" (p ': "primary" ': ps)
    client = listedFirst
    primary = listedSecond
    backups = consSuper $ consSuper refl

mainChoreo :: (KnownSymbols backups) => Choreo ("client" ': "primary" ': backups) ()
mainChoreo = do
  stateRef <- primary `locally` liftIO (newIORef "I'm Primary")
  bStRefs <- parallel0 backups \p -> liftIO (newIORef ("I'm " ++ toLocTm p))
  loop (stateRef, bStRefs)
  where
    primary :: forall ps p. Member "primary" (p ': "primary" ': ps)
    primary = listedSecond
    client :: forall ps. Member "client" ("client" ': ps)
    client = listedFirst
    backups = consSuper $ consSuper refl
    loop state = do
      request <- locally client $ read @Request <$> liftIO getLine
      response <- kvs request state
      void $ locallyM client $ (liftIO . putStrLn . ("> " ++) . show) <$> response
      loop state

main :: IO ()
main = runCLIIO $ runChoreo (mainChoreo @'["A", "B", "C", "D", "E", "F"])
