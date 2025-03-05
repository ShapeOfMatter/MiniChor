module Bookseller0Network where

{-
# Bookseller as individual network programs

This example implmenets the bookseller protocol as individual network
programs (non-choreographic way). See
[`bookseller-1-simple`](../bookseller-1-simple) for a description of
the protocol.

## Running the example

Same as [`bookseller-1-simple`](../bookseller-1-simple) but with `cabal run bookseller-0-network`.
-}

import CLI
import Choreography.Network
import Choreography.Network.Http
import Data (deliveryDateOf, priceOf)
import Data.Time
import System.Environment

buyer :: Network (CLI m) ()
buyer = do
  budget <- Run $ getInput @Int "Enter your total budget:"
  title <- Run $ getstr "Enter the title of the book to buy:"
  Send title ["seller"]
  price <- Recv "seller"
  if price <= budget
    then do
      Send True ["seller"]
      (deliveryDate :: Day) <- Recv "seller"
      Run $ putOutput "The book will be delivered on:" deliveryDate
    else do
      Send False ["seller"]
      Run $ putNote "The book's price is out of the budget"

seller :: Network (CLI m) ()
seller = do
  database <- Run $ getInput "Enter the book database (for `Read`):"
  title <- Recv "buyer"
  Send (database `priceOf` title) ["buyer"]
  decision <- Recv "buyer"
  if decision
    then do
      Send (database `deliveryDateOf` title) ["buyer"]
    else do
      return ()

main :: IO ()
main = do
  [loc] <- getArgs
  case loc of
    "buyer" -> runCLIIO $ runNetwork cfg "buyer" buyer
    "seller" -> runCLIIO $ runNetwork cfg "seller" seller
    _ -> error "unknown party"
  return ()
  where
    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343))
        ]
