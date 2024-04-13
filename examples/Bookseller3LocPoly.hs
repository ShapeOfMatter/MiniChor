{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Location-polymorphic bookseller

This example implements the location-polymorphic bookseller where the
buyer's location is abstracted away as an argument.

For a general description of the protocol, see [`bookseller-1-simple`](../bookseller-1-simple).

# Running the protocol

Same as [`bookseller-1-simple`](../bookseller-1-simple) but with `cabal run bookseller-3-loc-poly`.
-}

module Bookseller3LocPoly where

import Choreography
import Data.Time
import GHC.TypeLits
import System.Environment

$(mkLoc "buyer")
$(mkLoc "seller")
--type Participants = ["buyer", "seller"]

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes the name of the buyer as a parameter (`someBuyer`).
bookseller :: (KnownSymbol a) => Member a ps -> Choreo ("seller" ': ps) IO (Maybe Day @ a)
bookseller someBuyer = do
  let theBuyer = inSuper consSet someBuyer
  -- the buyer reads the title of the book and sends it to the seller
  title <- (theBuyer, \_ -> do
               putStrLn "Enter the title of the book to buy"
               getLine
           )
           ~~> seller

  -- the seller checks the price of the book and sends it to the buyer
  price <- (seller, \un -> return $ priceOf (un title)) ~~> theBuyer

  cond' (theBuyer, \un -> return $ un price < budget) \case
    True  -> do
      deliveryDate <- (seller, \un -> return $ deliveryDateOf (un title)) ~~> theBuyer

      theBuyer `locally` \un -> do
        putStrLn $ "The book will be delivered on " ++ show (un deliveryDate)
        return $ Just (un deliveryDate)

    False -> do
      theBuyer `locally` \_ -> do
        putStrLn "The book's price is out of the budget"
        return Nothing

budget :: Int
budget = 100

priceOf :: String -> Int
priceOf "Types and Programming Languages" = 80
priceOf "Homotopy Type Theory"            = 120
priceOf _ = error "unknown book"

deliveryDateOf :: String -> Day
deliveryDateOf "Types and Programming Languages" = fromGregorian 2022 12 19
deliveryDateOf "Homotopy Type Theory"            = fromGregorian 2023 01 01
deliveryDateOf _ = error "unknown book"

main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "buyer"  -> runChoreography cfg choreo "buyer"
    "seller" -> runChoreography cfg choreo "seller"
    _ -> error "unknown party"
  return ()
  where
    choreo = bookseller $ singleton buyer

    cfg = mkHttpConfig [ ("buyer",  ("localhost", 4242))
                       , ("seller", ("localhost", 4343))
                       ]
