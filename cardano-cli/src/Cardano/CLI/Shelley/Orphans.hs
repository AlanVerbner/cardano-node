{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CLI.Shelley.Orphans () where

import           Cardano.Prelude hiding (Ptr)

import           Control.Iterate.SetAlgebra as SetAlgebra
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Cardano.Crypto.Hash.Class (Hash (..))
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.API hiding (Hash, TxId, TxIn, TxOut)
import           Cardano.Api.Shelley hiding (Hash, TxId, TxIn, TxOut)

import           Shelley.Spec.Ledger.TxBody (TxId (TxId), TxIn (TxIn), TxOut (TxOut))
-- Shelley.Spec.Ledger.TxBody conflicts with typed api

instance Era era => ToJSONKey (TxIn era) where
  toJSONKey = ToJSONKeyText txInToText (Aeson.text . txInToText)

txInToText :: Era era => TxIn era -> Text
txInToText (TxIn (TxId txidHash) ix) =
  hashToText txidHash
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

deriving instance Era era => ToJSON (TxIn era)

instance Era era => ToJSON (TxOut era) where
  toJSON (TxOut addr amount) =
    Aeson.object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance ToJSON (OneEraHash xs) where
  toJSON = toJSON
         . Text.decodeLatin1
         . Base16.encode
         . SBS.fromShort
         . getOneEraHash


-- This instance is temporarily duplicated in cardano-config

instance ToJSON (HeaderHash blk) => ToJSON (Tip blk) where
  toJSON TipGenesis = object [ "genesis" .= True ]
  toJSON (Tip slotNo headerHash blockNo) =
    object
      [ "slotNo"     .= slotNo
      , "headerHash" .= headerHash
      , "blockNo"    .= blockNo
      ]

-- This instance is temporarily duplicated in cardano-config
deriving newtype instance ToJSON BlockNo

--
-- Simple newtype wrappers JSON conversion
--

deriving newtype instance ToJSON (TxId era)

deriving newtype instance Era era => ToJSON (UTxO era)

deriving newtype instance ToJSON (ShelleyHash era)
deriving newtype instance ToJSON (HashHeader era)

deriving newtype instance ToJSON (MetaDataHash era)
deriving newtype instance ToJSON LogWeight
deriving newtype instance ToJSON Likelihood
deriving newtype instance ToJSON (Stake StandardShelley)

deriving anyclass instance ToJSON (GenDelegs StandardShelley)
deriving anyclass instance ToJSON (IndividualPoolStake StandardShelley)
deriving anyclass instance ToJSON (ProposedPPUpdates StandardShelley)
deriving anyclass instance ToJSON (PPUPState StandardShelley)

deriving instance ToJSON Ptr
deriving instance ToJSON AccountState

deriving instance ToJSON (DPState StandardShelley)
deriving instance ToJSON (DState StandardShelley)
deriving instance ToJSON (FutureGenDeleg StandardShelley)
deriving instance ToJSON (InstantaneousRewards StandardShelley)
deriving instance ToJSON (SnapShot StandardShelley)
deriving instance ToJSON (SnapShots StandardShelley)
deriving instance ToJSON (NonMyopic StandardShelley)
deriving instance ToJSON (LedgerState StandardShelley)
deriving instance ToJSON (EpochState StandardShelley)
deriving instance ToJSON (PParams' StrictMaybe StandardShelley)
deriving instance ToJSON (PState StandardShelley)
deriving instance ToJSON (StakeReference StandardShelley)
deriving instance ToJSON (UTxOState StandardShelley)

deriving instance ToJSONKey Ptr
deriving instance ToJSONKey (FutureGenDeleg StandardShelley)

instance (ToJSONKey k, ToJSON v) => ToJSON (SetAlgebra.BiMap v k v) where
  toJSON = toJSON . SetAlgebra.forwards -- to normal Map
