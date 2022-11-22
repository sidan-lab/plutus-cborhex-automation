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


module GBTE.TreasuryValidator (validator) where

-- are all of these necessary?
import qualified    Ledger.Ada as Ada

import              Plutus.V1.Ledger.Value
import              Plutus.V2.Ledger.Api
import              Plutus.V2.Ledger.Contexts
import              Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import              Plutus.Script.Utils.V2.Typed.Scripts (ValidatorTypes, TypedValidator, mkTypedValidator, mkTypedValidatorParam, validatorScript, mkUntypedValidator)
import              PlutusTx
import              PlutusTx.Prelude    hiding (Semigroup (..), unless)
import              Prelude             (Show (..))
import qualified    Prelude                   as Pr

import GBTE.Types

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: TreasuryParam -> TreasuryDatum -> TreasuryAction -> ScriptContext -> Bool
treasuryValidator tp dat action ctx =
    case action of
        (Commit b)  ->      traceIfFalse "Access token missing from input"              inputHasAuthToken &&
                            traceIfFalse "Access token missing from contract output"    contractOutputHasAuthToken &&
                            traceIfFalse "Output Value must match BountyDetails"        (checkValueToBountyContract b) &&
                            traceIfFalse "Treasury must keep remaining lovelace"        (treasuryGetsLovelaceBack b) &&
                            traceIfFalse "Treasury must keep remaining tokens"          (treasuryGetsTokensBack b) &&
                            traceIfFalse "Not a valid bounty hash"                      (checkBountyHash b) &&
                            traceIfFalse "In and out datum must match"                  checkDatum
        Manage      ->      traceIfFalse "Only Issuer can change Treasury"              inputHasIssuerToken
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- Create a list of all CurrencySymbol in tx input
        inVals :: [CurrencySymbol]
        inVals = symbols $ valueSpent info

        -- case action -> Manage:
        -- If the TreasuryAction is "Manage", then input must have an IssuerToken
        inputHasIssuerToken :: Bool
        inputHasIssuerToken = tIssuerPolicyId tp `elem` inVals

        -- case action -> Commit:
        -- Check that list of CurrencySymbols includes Auth CurrencySymbol
        inputHasAuthToken :: Bool
        inputHasAuthToken = tAccessTokenPolicyId tp `elem` inVals

        -- The Value to be included in Bounty Contract UTXO
        toBountyContract :: Value
        toBountyContract = valueLockedBy info (bountyContractHash tp)

        -- Check that the Auth Token is sent to Bounty Contract UTXO
        contractOutputHasAuthToken :: Bool
        contractOutputHasAuthToken = tAccessTokenPolicyId tp `elem` symbols toBountyContract

        -- Check that the Value sent to Contract UTXO matches what is specified in the Redeemer
        checkValueToBountyContract :: BountyDetails -> Bool
        checkValueToBountyContract b =  Ada.getLovelace (Ada.fromValue toBountyContract) == lovelaceAmount b &&
                                    valueOf toBountyContract (tBountyTokenPolicyId tp) (tBountyTokenName tp) == tokenAmount b

        -- The UTXO input from Treasury
        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "treasury input missing"
            Just i  -> txInInfoResolved i

        -- The UTXO output back to Treasury
        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o -- There must be exactly ONE output UTXO
            _   -> traceError "expected exactly one treasury output"

        -- Values of each
        treasuryInputValue :: Value
        treasuryInputValue = txOutValue ownInput

        treasuryOutputValue :: Value
        treasuryOutputValue = txOutValue ownOutput

        -- Compare Values from and to Treasury to make sure that Treasury gets the right value back
        treasuryGetsLovelaceBack :: BountyDetails -> Bool
        treasuryGetsLovelaceBack b = lovelaceToTreasury == lovelaceFromTreasury - lovelaceToBounty
            where
                lovelaceFromTreasury = Ada.getLovelace (Ada.fromValue treasuryInputValue)
                lovelaceToTreasury = Ada.getLovelace (Ada.fromValue treasuryOutputValue)
                lovelaceToBounty = lovelaceAmount b

        treasuryGetsTokensBack :: BountyDetails -> Bool
        treasuryGetsTokensBack b = gimbalsToTreasury == gimbalsFromTreasury - gimbalsToBounty
            where
                gimbalsFromTreasury = valueOf treasuryInputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp)
                gimbalsToTreasury = valueOf treasuryOutputValue (tBountyTokenPolicyId tp) (tBountyTokenName tp)
                gimbalsToBounty = tokenAmount b

        -- Check that the bounty hash in redeemer matches one of the hashes in treasury datum.
        checkBountyHash :: BountyDetails -> Bool
        checkBountyHash b = (bountyHash b) `elem` (bountyHashList dat)

        checkDatum :: Bool
        checkDatum = txOutDatum ownInput == txOutDatum ownOutput


typedValidator :: TreasuryParam -> TypedValidator TreasuryTypes
typedValidator tp = go tp where
    go = mkTypedValidatorParam @TreasuryTypes
        $$(PlutusTx.compile [|| treasuryValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: TreasuryParam -> Validator
validator = validatorScript . typedValidator
