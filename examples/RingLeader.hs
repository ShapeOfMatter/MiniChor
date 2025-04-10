{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# This example was carried over from earlier work, likely HasChor. It doesn't yet have a unit test attached to it.

# Ring leader election

Experinmental implementaion of ring leader election.
-}

module RingLeader where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.TypeLits (KnownSymbol)
import System.Environment
-- an edge of the ring is represented as a tuple of two locaitons l and l' where
-- l is on the left of l'
data Edge g
  = forall l l'.
    (KnownSymbol l, KnownSymbol l') =>
    Edge (Member l g) (Member l' g)

-- a ring is a sequence of edges
type Ring g = [Edge g]

type Label = Int

type States g = Faceted g '[] (IORef Label)

ringLeader :: forall g. (KnownSymbols g) => Ring g -> Choreo g () -- g for graph
ringLeader r = initialize >>= (`loop` r)
  where
    initialize :: Choreo g (States g)
    initialize = parallel allOf $ getInput "Please input a label:" >>= liftIO . newIORef
    loop :: States g -> Ring g -> Choreo g ()
    loop states [] = loop states r -- not very safe!
    loop states (x : xs) = do
      finished <- talkToRight states x
      if finished
        then return ()
        else loop states xs
    get :: (KnownSymbol l) => States g -> Member l g -> Choreo g (Located '[l] Label)
    get states l = locallyM l $ (liftIO . readIORef) <$> localize l states
    put :: (KnownSymbol l) => States g -> Member l g -> Located '[l] Label -> Choreo g ()
    put states l val = locallyM_ l $ (\state v -> liftIO $ writeIORef state v) <$> localize l states <*> val
    talkToRight :: States g -> Edge g -> Choreo g Bool
    talkToRight states (Edge left right) = do
      ll <- get states left
      labelLeft <- (left, ll) ~> right @@ nobody
      labelRight <- get states right

      let finished = (==) <$> labelLeft <*> labelRight
      broadcast First (right @@ nobody) finished >>= \case
        True -> do
          locally_ right (liftIO $ putStrLn "I'm the leader")
          return True
        False -> do
          put states right (max <$> labelLeft <*> labelRight)
          return False

$(mkLoc "nodeA")
$(mkLoc "nodeB")
$(mkLoc "nodeC")
$(mkLoc "nodeD")

type Participants = ["nodeA", "nodeB", "nodeC", "nodeD"]

ring :: Ring Participants
ring =
  [ Edge nodeA nodeB,
    Edge nodeB nodeC,
    Edge nodeC nodeD,
    Edge nodeD nodeA
  ]

main :: IO ()
main = do
  [loc] <- getArgs
  runCLIIO $ runChoreography config (ringLeader ring) loc
  return ()
  where
    config =
      mkHttpConfig
        [ ("nodeA", ("localhost", 4242)),
          ("nodeB", ("localhost", 4343)),
          ("nodeC", ("localhost", 4444)),
          ("nodeD", ("localhost", 4545))
        ]
