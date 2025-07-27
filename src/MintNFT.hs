module MintNFT where

-- Standard Haskell imports
import Prelude

-- Define mock types for testing in GHCi
type PubKeyHash = String
type CurrencySymbol = String

-- Simulate the transaction info and script context
data TxInfo = TxInfo { signatories :: [PubKeyHash] }
data ScriptContext = ScriptContext { scriptContextTxInfo :: TxInfo }

-- Simulated txSignedBy function
txSignedBy :: TxInfo -> PubKeyHash -> Bool
txSignedBy info pkh = pkh `elem` signatories info

-- Minting policy logic (mocked)
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh _ ctx = txSignedBy (scriptContextTxInfo ctx) pkh

-- Mock policy function that simulates minting policy construction
policy :: PubKeyHash -> ((), ScriptContext) -> Bool
policy pkh (_unit, ctx) = mkPolicy pkh () ctx

-- Mock function to simulate a CurrencySymbol
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol pkh = "MockCurrencySymbolFor:" ++ pkh

-- Example usage in GHCi:
-- let ctx = ScriptContext (TxInfo ["abc123"])
-- policy "abc123" ((), ctx)  -- should return True
-- policy "xyz999" ((), ctx)  -- should return False
-- curSymbol "abc123"         -- should return "MockCurrencySymbolFor:abc123"
