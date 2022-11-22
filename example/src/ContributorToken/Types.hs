{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module ContributorToken.Types where

import              GHC.Generics                (Generic)
import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import  qualified   PlutusTx
import              PlutusTx.Prelude    hiding  (Semigroup (..), unless)
import              Prelude                     (Show (..))
import  qualified   Prelude                 as  Pr
import              Data.Aeson                  (FromJSON, ToJSON)
import              Dev.DevDefaultOrphans()

-- ReferenceParams
data ReferenceParams = ReferenceParams
    { adminPolicyID       :: !CurrencySymbol
    , contributorPolicyID :: !CurrencySymbol
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''ReferenceParams

-- ContributorDatum
-- Refactor with a Map of MasteryLevels, which is more extensible
-- Once MVP is working, add something like "Completed Project Hashes"
data ContributorDatum = ContributorDatum
    { masteryLevels     :: Map BuiltinByteString Integer
    , contribCount      :: !Integer
    , alias             :: !BuiltinByteString
    }

PlutusTx.unstableMakeIsData ''ContributorDatum
PlutusTx.makeLift ''ContributorDatum


-- ReferenceRedeemer
data ReferenceRedeemer = UpdateDatum | RemoveReference
    deriving Show

PlutusTx.makeIsDataIndexed ''ReferenceRedeemer [('UpdateDatum, 0), ('RemoveReference, 1)]
PlutusTx.makeLift ''ReferenceRedeemer


data ReferenceTypes
instance ValidatorTypes ReferenceTypes where
    type DatumType ReferenceTypes = ContributorDatum
    type RedeemerType ReferenceTypes = ReferenceRedeemer
