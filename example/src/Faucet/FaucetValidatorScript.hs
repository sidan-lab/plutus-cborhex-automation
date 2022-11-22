{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Faucet.FaucetValidatorScript
  ( FaucetParams (..)
  , validator
  ) where


import qualified    Ledger (contains)
import qualified    Ledger.Ada as Ada
import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              Prelude                     (Show (..))
import  qualified   Prelude                 as  Pr
import              GHC.Generics                (Generic)

import              Schema                      (ToSchema)
import              Data.Aeson                  (FromJSON, ToJSON)
import              SIDANDefaultOrphans         ()

-- Simple Faucet validator script

-- Usage:
-- Expect one utxo at each contract.
-- Take that utxo as input
-- Create a new utxo with the "change" as output

-- This contract provides an example of using Validator Parameters.

-- For now, the Datum and Redeemer are not used in contract logic
-- Transactions will still have to match the type Integer for Datum and Redeemer
-- The context matters: we want to see that the PPBLSummer2022 token is in the transaction inputs and outputs.

data FaucetParams = FaucetParams
  { accessTokenSymbol   :: CurrencySymbol
  , accessTokenName     :: TokenName
  , faucetTokenSymbol   :: CurrencySymbol
  , faucetTokenName     :: TokenName
  , withdrawalAmount    :: Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''FaucetParams

newtype FaucetRedeemer = FaucetRedeemer {senderPkh :: PubKeyHash}

PlutusTx.unstableMakeIsData ''FaucetRedeemer
PlutusTx.makeLift ''FaucetRedeemer

{-# INLINEABLE faucetValidator #-}
faucetValidator :: FaucetParams -> Integer -> FaucetRedeemer -> ScriptContext -> Bool
faucetValidator faucet _ receiver ctx =  traceIfFalse "Input needs PPBL2022AccessToken"           inputHasAccessToken &&
                                                  traceIfFalse "PPBL2022AccessToken must return to sender" outputHasAccessToken &&
                                                  traceIfFalse "Faucet token must be distributed to sender" outputHasFaucetToken &&
                                                  traceIfFalse "Must return remaining tokens to contract"   faucetContractGetsRemainingTokens &&
                                                  traceIfFalse "Do we need to check datum"                  checkDatumIsOk
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    receiverPkh :: PubKeyHash
    receiverPkh = senderPkh receiver

    allTokens :: [CurrencySymbol]
    allTokens = symbols $ valueSpent info

    inputHasAccessToken :: Bool
    inputHasAccessToken = (accessTokenSymbol faucet) `elem` allTokens

    valueToReceiver :: Value
    valueToReceiver = valuePaidTo info receiverPkh

    outputHasAccessToken :: Bool
    outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol faucet) (accessTokenName faucet)) >= 1

    outputHasFaucetToken :: Bool
    outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol faucet) (faucetTokenName faucet)) >= (withdrawalAmount faucet)

    -- The UTXO input from Faucet
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i  -> txInInfoResolved i

    -- The UTXO output back to Faucet
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- There must be exactly ONE output UTXO
        _   -> traceError "expected exactly one faucet output"

    faucetInputValue :: Value
    faucetInputValue = txOutValue ownInput

    faucetOutputValue :: Value
    faucetOutputValue = txOutValue ownOutput

    faucetContractGetsRemainingTokens :: Bool
    faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol faucet) (faucetTokenName faucet)) - (withdrawalAmount faucet) <= (valueOf faucetOutputValue (faucetTokenSymbol faucet) (faucetTokenName faucet))

    checkDatumIsOk :: Bool
    checkDatumIsOk = True

data FaucetTypes
instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
    type RedeemerType FaucetTypes = FaucetRedeemer

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator = go where
    go = mkTypedValidatorParam @FaucetTypes
        $$(PlutusTx.compile [|| faucetValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator
