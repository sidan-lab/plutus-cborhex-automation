{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies #-}

module GBTE.Types
    ( TreasuryParam (..)
    , TreasuryDatum (..)
    , BountyDetails (..)
    , TreasuryAction (..)
    , BountyAction (..)
    , BountyParam (..)
    , BountyEscrowDatum (..)
    , TreasuryTypes
    , EscrowTypes
    ) where

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

import           SIDANDefaultOrphans()
import           Data.Aeson (FromJSON, ToJSON)

data TreasuryParam = TreasuryParam
    { tIssuerPolicyId        :: CurrencySymbol
    , tAccessTokenPolicyId   :: CurrencySymbol
    , bountyContractHash     :: ValidatorHash
    , tBountyTokenPolicyId   :: CurrencySymbol
    , tBountyTokenName       :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''TreasuryParam

-- consider representing Issuer with a token, instead of PKH
data TreasuryDatum = TreasuryDatum
  { bountyHashList  :: [BuiltinByteString]
  , issuerTokenName :: TokenName
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''TreasuryDatum

data BountyDetails = BountyDetails
  { contributorPkh      :: PubKeyHash
  , lovelaceAmount      :: Integer
  , tokenAmount         :: Integer
  , expirationTime      :: POSIXTime
  , bountyHash          :: BuiltinByteString
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq BountyDetails where
  {-# INLINABLE (==) #-}
  BountyDetails cP lA tA eT bH == BountyDetails cP' lA' tA' eT' bH' =
    (cP == cP') && (lA == lA') && (tA == tA') && (eT == eT') && (bH == bH')

PlutusTx.unstableMakeIsData ''BountyDetails
PlutusTx.makeLift ''BountyDetails

data TreasuryAction = Commit BountyDetails | Manage
    deriving Show

PlutusTx.makeIsDataIndexed ''TreasuryAction [('Commit, 0), ('Manage, 1)]
PlutusTx.makeLift ''TreasuryAction

data TreasuryTypes
instance ValidatorTypes TreasuryTypes where
    type DatumType TreasuryTypes = TreasuryDatum
    type RedeemerType TreasuryTypes = TreasuryAction

data BountyEscrowDatum = BountyEscrowDatum
  { bedContributorPkh      :: PubKeyHash
  , bedLovelaceAmount      :: Integer
  , bedTokenAmount         :: Integer
  , bedExpirationTime      :: POSIXTime
  , bedBountyHash          :: BuiltinByteString
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

instance Eq BountyEscrowDatum where
  {-# INLINABLE (==) #-}
  BountyEscrowDatum bCP bLA bTA bET bBH == BountyEscrowDatum bCP' bLA' bTA' bET' bBH' =
    (bCP == bCP') && (bLA == bLA') && (bTA == bTA') && (bET == bET') && (bBH == bBH')

    -- Alternative way of comparisons
    -- a == b = (bedIssuerPkh       a == bedIssuerPkh      b) &&
    --          (bedContributorPkh  a == bedContributorPkh b) &&
    --          (bedLovelaceAmount  a == bedLovelaceAmount b) &&
    --          (bedTokenAmount     a == bedTokenAmount    b) &&
    --          (bedExpirationTime  a == bedExpirationTime b)

PlutusTx.unstableMakeIsData ''BountyEscrowDatum
PlutusTx.makeLift ''BountyEscrowDatum

data BountyParam = BountyParam
    { bountyTokenPolicyId     :: CurrencySymbol
    , bountyTokenName         :: TokenName
    , accessTokenPolicyId     :: CurrencySymbol
    , treasuryIssuerPolicyId  :: CurrencySymbol
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''BountyParam

data BountyAction = Cancel | Distribute | Update
  deriving Show

PlutusTx.makeIsDataIndexed ''BountyAction [('Cancel, 0), ('Distribute, 1), ('Update, 2)]
PlutusTx.makeLift ''BountyAction

data EscrowTypes
instance ValidatorTypes EscrowTypes where
    type DatumType EscrowTypes = BountyEscrowDatum
    type RedeemerType EscrowTypes = BountyAction
