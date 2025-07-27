module Marketplace where

-- Standard imports
import Prelude

-- Mock types
type PubKeyHash = String
type Integer = Int
type Price = Integer
type RoyaltyBps = Integer
type CurrencySymbol = String
type TokenName = String

-- Mock Datum
data MarketDatum = MarketDatum
  { seller      :: PubKeyHash
  , price       :: Price
  , creator     :: PubKeyHash
  , royaltyBps  :: RoyaltyBps
  } deriving (Show)

-- Mock Value and Address system
type Value = [(CurrencySymbol, TokenName, Integer)]

data TxOut = TxOut
  { txOutAddress :: String
  , txOutValue   :: Value
  } deriving (Show)

data TxInfo = TxInfo
  { txInfoOutputs :: [TxOut]
  , payments      :: [(PubKeyHash, Integer)]
  } deriving (Show)

data ScriptContext = ScriptContext
  { scriptContextTxInfo :: TxInfo
  } deriving (Show)

-- Simulated constants
adaSymbol :: CurrencySymbol
adaSymbol = "lovelace"

adaToken :: TokenName
adaToken = ""

-- Helper functions
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf val sym tok =
  sum [ amount | (s, t, amount) <- val, s == sym, t == tok ]

valuePaidTo :: TxInfo -> PubKeyHash -> Integer
valuePaidTo info pkh =
  sum [ amt | (pk, amt) <- payments info, pk == pkh ]

-- Simulated txOutAddress to PubKeyHash conversion
toPubKeyHash :: String -> Maybe PubKeyHash
toPubKeyHash addr = Just addr  -- mock: assume all addresses are pub key hashes

-- Simulated traceIfFalse (just enforces condition in GHCi)
traceIfFalse :: String -> Bool -> Bool
traceIfFalse _ cond = cond

divide :: Integer -> Integer -> Integer
divide a b = a `div` b

-- Validator logic
mkValidator :: MarketDatum -> () -> ScriptContext -> Bool
mkValidator dat _ ctx =
    traceIfFalse "Buyer did not pay enough"        paidEnough       &&
    traceIfFalse "Seller not paid"                 sellerPaid       &&
    traceIfFalse "Creator royalty not paid"        creatorPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    expectedPrice :: Integer
    expectedPrice = price dat

    royaltyAmount :: Integer
    royaltyAmount = (expectedPrice * royaltyBps dat) `divide` 10000

    sellerAmount :: Integer
    sellerAmount = expectedPrice - royaltyAmount

    paidTo :: PubKeyHash -> Integer
    paidTo pkh = sum
      [ valueOf val adaSymbol adaToken
      | TxOut addr val <- txInfoOutputs info
      , Just p <- [toPubKeyHash addr], p == pkh
      ]

    paidEnough :: Bool
    paidEnough = valuePaidTo info (seller dat) >= expectedPrice

    sellerPaid :: Bool
    sellerPaid = paidTo (seller dat) >= sellerAmount

    creatorPaid :: Bool
    creatorPaid = paidTo (creator dat) >= royaltyAmount

-- Example test
-- ghci> let datum = MarketDatum "seller1" 10000 "creator1" 1000
-- ghci> let out1 = TxOut "seller1" [("lovelace", "", 9000)]
-- ghci> let out2 = TxOut "creator1" [("lovelace", "", 1000)]
-- ghci> let ctx = ScriptContext (TxInfo [out1, out2] [("seller1", 10000)])
-- ghci> mkValidator datum () ctx
-- True
