{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Bookseller1Simple where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Monad (void)
import Data (deliveryDateOf, priceOf)
import System.Environment

$(mkLoc "buyer")
$(mkLoc "seller")

type Participants = ["buyer", "seller"]

-- | `bookseller` is a choreography that implements the bookseller protocol.
bookseller :: Choreo Participants ()
bookseller = do
  database <- seller `locally` getInput "Enter the book database (for `Read`):"
  buyer_budget <- buyer `locally` getInput "Enter your total budget:"
  title <- buyer `locally` getstr "Enter the title of the book to buy:"

  title' <- (buyer, title) ~> seller @@ nobody
  price <- locally2 seller  (seller, database) (seller, title') \d t -> return $ priceOf d t
  price' <- (seller, price) ~> buyer @@ nobody
  decision <- locally2 buyer (buyer, price') (buyer, buyer_budget) \p b -> return $ p <= b

  broadcast (buyer, decision) >>= \case
    True -> do
      deliveryDate <- locally2 seller (seller, database) (seller, title') \d t -> return $ deliveryDateOf d t
      deliveryDate' <- (seller, deliveryDate) ~> buyer @@ nobody
      void $ locally1 buyer (buyer, deliveryDate') \dd -> putOutput "The book will be delivered on:" dd
    False -> do
      buyer `locally_` putNote "The book's price is out of the budget"

-- removed bookseller' because it's only distinguished by ~~>, which is sugar we no longer have.

main :: IO ()
main = do
  [loc] <- getArgs
  delivery <- case loc of
    "buyer" -> runCLIIO $ runChoreography cfg bookseller "buyer"
    "seller" -> runCLIIO $ runChoreography cfg bookseller "seller"
    _ -> error "unknown party"
  print delivery
  where
    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343))
        ]
