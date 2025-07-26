-- File: src/Voting/Validator.hs
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Voting.Validator where

import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Api
import           PlutusTx
import           Voting.Types

{-# INLINABLE mkValidator #-}
mkValidator :: VoteDatum -> VoteRedeemer -> ScriptContext -> Bool
mkValidator dat red _ctx = case red of
    Commit -> traceIfFalse "Already revealed" (not (revealed dat))
    Reveal vote nonce ->
        let recomputed = sha2_256 (vote <> nonce)
        in  traceIfFalse "Hash mismatch" (recomputed == committedHash dat)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: ValidatorHash
valHash = validatorHash validator

scrAddress :: Address
scrAddress = scriptAddress validator
