{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module CLI where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), StateT (runStateT), lift)
import Data.Typeable (Typeable, typeRep)
import Text.Read (readMaybe)

type Context = String

data CLI m a where
  GetStr :: Context -> CLI m String
  PutStr :: Context -> String -> CLI m ()
  Internal :: m a -> CLI m a
  Return :: a -> CLI m a
  Bind :: CLI m a -> (a -> CLI m b) -> CLI m b

instance Functor (CLI m) where
  fmap f net = Bind net (Return . f ) 

instance Applicative (CLI m) where
  pure = Return
  netf <*> neta = Bind netf (\f -> Bind neta (Return . f))

instance Monad (CLI m) where
  (>>=) = Bind

instance (MonadIO m) => MonadIO (CLI m) where
  liftIO = Internal . liftIO

runCLIIO :: forall m a. (MonadIO m) => CLI m a -> m a
runCLIIO = handler
  where
    handler :: CLI m b -> m b
    handler (GetStr prompt) = liftIO $ putStrLn prompt >> getLine
    handler (PutStr context l) = liftIO $ putStrLn $ context ++ " " ++ l
    handler (Internal m) = m
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont

getstr :: Context -> CLI m String
getstr = GetStr

getln :: CLI m String
getln = getstr ""

getInput :: forall a m. (Read a, Typeable a) => Context -> CLI m a
getInput context = do
  str <- getstr context
  case readMaybe str of
    Just a -> return a
    a@Nothing -> error $ "Failed to read \"" ++ str ++ "\" as a " ++ show (typeRep a)

putstr :: Context -> String -> CLI m ()
putstr = PutStr

putNote :: Context -> CLI m ()
putNote = (`putstr` "")

putOutput :: (Show a) => Context -> a -> CLI m ()
putOutput context a = putstr context $ show a

data TTYEnv = TTYEnv
  { inputs :: [String],
    outputs :: [String]
  }

runCLIStateful :: forall m a. (MonadFail m, MonadIO m) => [String] -> CLI m a -> m ([String], a)
runCLIStateful ins tma = do
  (a, e) <- runStateT stateful TTYEnv {inputs = ins, outputs = []}
  return (reverse $ outputs e, a)
  where
    stateful :: StateT TTYEnv m a
    stateful = handler tma
    handler :: forall b. CLI m b -> StateT TTYEnv m b
    handler (GetStr c) = do
      env@TTYEnv {inputs} <- get
      case inputs of
        ln : lns -> do
          put env {inputs = lns}
          return ln
        [] -> error $ "No input to go with prompt " ++ show c ++ "."
    handler (PutStr _ o) = unless (null o) $ do
      env@TTYEnv {outputs = os} <- get
      put env {outputs = o : os}
    handler (Internal m) = lift m
    handler (Return a) = pure a
    handler (Bind m cont) = handler m >>= handler . cont
