-- File: src/Voting/Types.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Voting.Types where

import           PlutusTx.Prelude
import           PlutusTx (makeIsDataIndexed, makeLift)
import           GHC.Generics (Generic)
import           Prelude (Show)

-- | Voting datum holds the commitment and whether it has been revealed
data VoteDatum = VoteDatum
    { committedHash :: BuiltinByteString
    , revealed      :: Bool
    } deriving (Generic, Show)

-- | Redeemer for two actions: Commit or Reveal
data VoteRedeemer = Commit | Reveal BuiltinByteString BuiltinByteString
-- Reveal vote and nonce

makeIsDataIndexed ''VoteDatum [('VoteDatum, 0)]
makeIsDataIndexed ''VoteRedeemer [('Commit, 0), ('Reveal, 1)]
makeLift ''VoteDatum
makeLift ''VoteRedeemer
