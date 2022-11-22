{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-context #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

module TestingValidator where

-- import ThreadToken.ThreadToken
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Prelude (IO, Semigroup (..), Show (..), String)
import Ledger (POSIXTime, to, from, contains)
import Ledger.Address (PaymentPubKeyHash, Address, unPaymentPubKeyHash)
import Ledger.Ada as Ada
-- import Ledger.Value
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V1.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V1.Ledger.Contexts as Plutus
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators as V1V
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude as PPrelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Data.Bool (Bool(True))
import Playground.Contract (ToSchema)

import qualified Plutus.V1.Ledger.Value            as PlutusV1
import qualified Plutus.V2.Ledger.Api              as PlutusV2
-- import DevelopmentSourceCode

data TestParam = TestParam {
  testTN     :: PlutusV2.TokenName,
  testCS     :: PlutusV2.CurrencySymbol,
  testAC     :: PlutusV1.AssetClass,
  testTime   :: PlutusV2.POSIXTime,
  testAddr   :: PlutusV2.Address,
  testPkh    :: PlutusV2.PubKeyHash,
  testVH     :: PlutusV2.ValidatorHash,
  testSC     :: PlutusV2.StakingCredential,
  testNumber :: Integer,
  testPpkh   :: PaymentPubKeyHash
} deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''TestParam
PlutusTx.makeIsDataIndexed ''TestParam [('TestParam,0)]


{-# INLINEABLE mkValidator #-}
mkValidator :: TestParam -> Integer -> () -> Plutus.ScriptContext -> Bool
mkValidator param dat _ _ = dat == testNumber param

validator :: TestParam -> Plutus.Validator
validator param = Plutus.mkValidatorScript
  ($$(PlutusTx.compile [||wrap||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
  where
    wrap = Scripts.mkUntypedValidator . mkValidator

-- callAddress :: TestParam -> Address
-- callAddress = mkValidatorAddress . validator
