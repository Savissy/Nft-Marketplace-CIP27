-- File: src/Voting/Export.hs
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Voting.Export where

import           PlutusTx.Prelude
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Scripts
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Voting.Validator
import           Prelude (IO)
import           Codec.Serialise (serialise)
import           System.IO (writeFile)

writeValidator :: IO ()
writeValidator = do
    let script = validator
        sbs = SBS.toShort . LBS.toStrict $ serialise script
    writeFile "vote-validator.plutus" (show sbs)
