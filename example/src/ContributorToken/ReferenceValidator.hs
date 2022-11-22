{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NoImplicitPrelude     #-}


module ContributorToken.ReferenceValidator (validator) where

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

import ContributorToken.Types

{-# INLINEABLE referenceValidator #-}
referenceValidator :: ReferenceParams -> ContributorDatum -> ReferenceRedeemer -> ScriptContext -> Bool
referenceValidator rp dat action ctx =
    case action of
        UpdateDatum         -> traceIfFalse "Admin token missing from Tx"           txHasAdminToken &&
                               traceIfFalse "ReferenceToken must return to UTxO"    refTokenToContract &&
                               traceIfFalse "Inline datum is required"              checkDatum
        RemoveReference     -> traceIfFalse "Admin token missing from Tx"           txHasAdminToken

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- When MVP is working, add multi-sig here (or just remove this and assume Admin token is used correctly.)
        adminPkh :: PubKeyHash
        adminPkh = case txInfoSignatories info of
            [pkh] -> pkh
            _     -> traceError "Only Admin should sign tx"

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "Reference input missing"
            Just i  -> txInInfoResolved i

        -- The UTXO output back to Reference Address
        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o -- There must be exactly ONE output UTXO
            _   -> traceError "expected exactly one Reference output"

        inVals :: [CurrencySymbol]
        inVals = symbols $ valueSpent info

        inputHasAdminToken :: Bool
        inputHasAdminToken = adminPolicyID rp `elem` inVals

        valueToAdmin :: Value
        valueToAdmin = valuePaidTo info adminPkh

        outputToAdminHasAdminToken :: Bool
        outputToAdminHasAdminToken = adminPolicyID rp `elem` symbols valueToAdmin

        txHasAdminToken :: Bool
        txHasAdminToken = inputHasAdminToken && outputToAdminHasAdminToken

        -- Implement Me!
        refTokenToContract :: Bool
        refTokenToContract = True

        checkDatum :: Bool
        checkDatum = case txOutDatum ownOutput of
            i  -> True
            _  -> traceError "Output datum missing"


typedValidator :: ReferenceParams -> TypedValidator ReferenceTypes
typedValidator rp = go rp where
    go = mkTypedValidatorParam @ReferenceTypes
        $$(PlutusTx.compile [|| referenceValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = mkUntypedValidator

validator :: ReferenceParams -> Validator
validator = validatorScript . typedValidator
