{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-
# Maximally fancy bookseller
-}

module BooksellerFancy where

import CLI
import Choreography
import Choreography.Network.Http
import Control.Monad (void)
import Data (deliveryDateOf, priceOf)
import System.Environment

$(mkLoc "buyer")
$(mkLoc "seller")

-- | `bookseller` is a choreography that implements the bookseller protocol.
-- This version takes a choreography `mkDecision` that implements the decision making process.
bookseller ::
  forall supporters .
  (KnownSymbols supporters) =>
  (Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (Located '["buyer"] Bool)) ->
  Choreo ("buyer" ': "seller" ': supporters) ()
bookseller mkDecision = do
  database <- seller `locally` getInput "Enter the book database (for `Read`):"
  title <- (buyer, getstr "Enter the title of the book to buy:") -~> seller @@ nobody

  -- the seller checks the price of the book and sends it to the buyer
  price' <- congruently2 (seller @@ nobody) (refl, database) (refl, title) priceOf
  price <- (seller, price') ~> buyer @@ nobody

  -- the buyer and supporters (transactors) make a decision using the `mkDecision` choreography
  decision <- enclave transactors (mkDecision price)
                  >>= flatten (buyer @@ nobody) explicitSubset allOf

  -- if the buyer decides to buy the book, the seller sends the delivery date to the buyer
  _ <-
    enclave buyerAndSeller $
      broadcast (buyer, decision) >>= \case
        True -> do
          dd <- locally2 seller (seller, database) (seller, title) (\d t -> return $ deliveryDateOf d t)
          deliveryDate <- (seller, dd) ~> buyer @@ nobody
          void $ locally1 buyer (buyer, deliveryDate) (putstr "The book will be delivered on:" . show)
        False -> do
          void $ buyer `locally` putNote "The book's price is out of the budget"

  return ()
  where
    transactors :: Subset ("buyer" ': supporters) ("buyer" ': "seller" ': supporters)
    transactors = explicitMember @@ (consSuper . consSuper $ refl)

    buyerAndSeller :: Subset '["buyer", "seller"] ("buyer" ': "seller" ': supporters)
    buyerAndSeller = explicitSubset

-- | `mkDecision1` checks if buyer's budget is greater than the price of the book
mkDecision1 :: Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (Located '["buyer"] Bool)
mkDecision1 price = do
  budget <- buyer `locally` getInput "What are you willing to pay?"
  locally2 buyer (buyer, price) (buyer, budget) \p b -> return $ p <= b

-- | `mkDecision2` asks supporters how much they're willing to contribute and checks
-- if the buyer's budget is greater than the price of the book minus all supporters' contribution
mkDecision2 :: forall supporters . (KnownSymbols supporters) => Located '["buyer"] Int -> Choreo ("buyer" ': supporters) (Located '["buyer"] Bool)
mkDecision2 price = do
  budget <- buyer `locally` getInput "What are you willing to pay?"

  contribs <- fanIn @supporters explicitSubset $ \supporter ->
    (Later supporter, getInput "How much you're willing to contribute?") -~> buyer @@ nobody
  contrib <- locally1 buyer (buyer, contribs) (return . sum)
  locally3 buyer (buyer, price) (buyer, budget) (buyer, contrib) \p b c -> return $ p <= b + c

main :: IO ()
main = do
  [loc] <- getArgs
  _ <- case loc of
    "buyer" -> runCLIIO $ runChoreography cfg choreo "buyer"
    "seller" -> runCLIIO $ runChoreography cfg choreo "seller"
    "buyer2" -> runCLIIO $ runChoreography cfg choreo "buyer2"
    "buyer3" -> runCLIIO $ runChoreography cfg choreo "buyer3"
    _ -> error "unknown party"
  return ()
  where
    choreo = bookseller @["buyer2", "buyer3"] mkDecision2

    cfg =
      mkHttpConfig
        [ ("buyer", ("localhost", 4242)),
          ("seller", ("localhost", 4343)),
          ("buyer2", ("localhost", 4444)),
          ("buyer3", ("localhost", 45454))
        ]
