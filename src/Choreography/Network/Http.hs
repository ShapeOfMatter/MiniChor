-- | This module implments the HTTP message transport backend for the `Network`
-- monad.
module Choreography.Network.Http where

import Choreography.Locations
import Choreography.Network
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Either (lefts)
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import Servant.Server (Handler, Server, serve)

-- * Http configuration

-- | A backend for running `Network` behaviors over HTTP.
--   The configuration specifies how locations are mapped to network hosts and ports.
newtype HttpConfig = HttpConfig
  { locToUrl :: HashMap LocTm BaseUrl
  }

-- | The address of a party/location.
type Host = String

-- | The port of a party/location.
type Port = Int

-- | Create a HTTP backend configuration from a association list that maps
--   locations to network hosts and ports.
mkHttpConfig :: [(LocTm, (Host, Port))] -> HttpConfig
mkHttpConfig = HttpConfig . HashMap.fromList . fmap (fmap f)
  where
    f :: (Host, Port) -> BaseUrl
    f (host, port) =
      BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = host,
          baseUrlPort = port,
          baseUrlPath = ""
        }

-- | The list of locations known to a backend.
locs :: HttpConfig -> [LocTm]
locs = HashMap.keys . locToUrl

-- * Receiving channels

-- | The channels a location uses to recieve messages from various peers.
type RecvChans = HashMap LocTm (Chan String)

-- | Make the channels that will be used to recieve messages.
mkRecvChans :: HttpConfig -> IO RecvChans
mkRecvChans cfg = foldM f HashMap.empty (locs cfg)
  where
    f ::
      HashMap LocTm (Chan String) ->
      LocTm ->
      IO (HashMap LocTm (Chan String))
    f hm l = do
      c <- newChan
      pure $ HashMap.insert l c hm

-- * HTTP backend

-- | A "Servant.API" API.
type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

-- | Run a `Network` behavior, using the provided HTTP backend.
runNetworkHttp :: (MonadIO m) => HttpConfig -> LocTm -> Network m a -> m a
runNetworkHttp cfg self prog = do
  mgr <- liftIO $ newManager defaultManagerSettings
  chans <- liftIO $ mkRecvChans cfg
  recvT <- liftIO $ forkIO (recvThread cfg chans)
  result <- runNetworkMain mgr chans prog
  liftIO $ threadDelay 1000000 -- wait until all outstanding requests to be completed
  liftIO $ killThread recvT
  pure result
  where
    runNetworkMain :: (MonadIO m) => Manager -> RecvChans -> Network m a -> m a
    runNetworkMain mgr chans = handler
      where
        handler :: (MonadIO m) => Network m a -> m a
        handler (Run m) = m
        handler (Send a ls) = liftIO $ do
          res <- mapM (\l -> runClientM (send self $ show a) (mkClientEnv mgr (locToUrl cfg ! l))) ls
          case lefts res of
            [] -> pure ()
            errors -> putStrLn $ "Errors : " <> show errors
        handler (Recv l) = liftIO $ read <$> readChan (chans ! l)
        handler (Return a) = pure a
        handler (Bind m cont) = handler m >>= handler . cont

    api :: Proxy API
    api = Proxy

    send :: LocTm -> String -> ClientM NoContent
    send = client api

    server :: RecvChans -> Server API
    server chans = handler
      where
        handler :: LocTm -> String -> Handler NoContent
        handler rmt msg = do
          liftIO $ writeChan (chans ! rmt) msg
          pure NoContent

    recvThread :: HttpConfig -> RecvChans -> IO ()
    recvThread cfg' chans = run (baseUrlPort $ locToUrl cfg' ! self) (serve api $ server chans)

instance Backend HttpConfig where
  runNetwork = runNetworkHttp
