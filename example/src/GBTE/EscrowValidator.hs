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

-- PlutusV2
module GBTE.EscrowValidator (validator) where

import qualified    Ledger (contains)
import qualified    Ledger.Ada as Ada
import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)

import              GBTE.Types

-- TODO add logic so that deadline can only be extended, but not shortened.

{-# INLINEABLE escrowValidator #-}
escrowValidator :: BountyParam -> BountyEscrowDatum -> BountyAction -> ScriptContext -> Bool
escrowValidator bp dat action ctx =
  case action of
    Cancel      ->  traceIfFalse "Only Issuer can Cancel Bounty"                inputHasIssuerToken &&
                    traceIfFalse "Can only cancel bounty after deadline"        deadlineReached
    Update      ->  traceIfFalse "Only Issuer can Update Bounty"                inputHasIssuerToken &&
                    traceIfFalse "Update must create one new Bounty UTXO"       createsContinuingBounty &&
                    traceIfFalse "Output UTXO value must be geq datum specs"    outputFulfillsValue
    Distribute  ->  traceIfFalse "Issuer must sign to distribute bounty"        inputHasIssuerToken &&
                    traceIfFalse "Contributor must receive full bounty values"  outputFulfillsBounty
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    bCursym :: CurrencySymbol
    bCursym = bountyTokenPolicyId bp

    bTokenN :: TokenName
    bTokenN = bountyTokenName bp

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that input has Issuer Token
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = treasuryIssuerPolicyId bp `elem` inVals

    deadlineReached :: Bool
    deadlineReached = Ledger.contains (from $ bedExpirationTime dat) $ txInfoValidRange info

    -- Update means that a UTXO must be left at contract address
    outputsToContract :: [TxOut]
    outputsToContract = getContinuingOutputs ctx

    createsContinuingBounty :: Bool
    createsContinuingBounty = length outputsToContract == 1

    outputContainsValue :: [TxOut] -> Bool
    outputContainsValue [x]   = valueOf (txOutValue x) bCursym bTokenN >= bedTokenAmount dat &&
                                Ada.getLovelace (Ada.fromValue $ txOutValue x) >= bedLovelaceAmount dat
    outputContainsValue _     = False

    outputFulfillsValue :: Bool
    outputFulfillsValue = outputContainsValue outputsToContract

    valueToContributor :: Value
    valueToContributor = valuePaidTo info $ bedContributorPkh dat

    -- The value sent to Contributor must be at least the amount specified by bounty
    -- contributor must get tokenAmount bp of gimbals and lovelaceAmount bp...
    outputFulfillsBounty :: Bool
    outputFulfillsBounty = valueOf valueToContributor bCursym bTokenN >= bedTokenAmount dat &&
                           Ada.getLovelace (Ada.fromValue valueToContributor) >= bedLovelaceAmount dat

    ownInputVal :: Value
    ownInputVal = case findOwnInput ctx of
                Just iv -> txOutValue $ txInInfoResolved iv
                Nothing -> error ()

typedValidator :: BountyParam -> TypedValidator EscrowTypes
typedValidator bp = go bp where
    go = mkTypedValidatorParam @EscrowTypes
        $$(PlutusTx.compile [|| escrowValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: BountyParam -> Validator
validator = validatorScript . typedValidator
