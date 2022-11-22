{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- from https://github.com/james-iohk/plutus-scripts/blob/master/src/TokenNamePolicy.hs

module GBTE.ApprovalMinter
  ( serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2
  )
where

import           GHC.Generics                         (Generic)
import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Maybe                           (fromJust)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (getPir)
import qualified PlutusTx
import              PlutusTx.Prelude    hiding  (Semigroup (..), unless)
import              Prelude                     (Show (..), FilePath, IO)
import  qualified   Prelude                 as  Pr

data ApprovalMinterParams = ApprovalMinterParams
  { treasuryIssuerPID :: !CurrencySymbol
  , gbteInstance      :: !Integer
  } deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.unstableMakeIsData ''ApprovalMinterParams
PlutusTx.makeLift ''ApprovalMinterParams

{-# INLINEABLE tokenNamePolicyV2 #-}
tokenNamePolicyV2 :: ApprovalMinterParams -> TokenName -> PlutusV2.ScriptContext -> Bool
tokenNamePolicyV2 amp tn ctx = traceIfFalse "wrong token name" checkTokenName
    where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    -- Create a list of all CurrencySymbol in tx input
    inVals :: [CurrencySymbol]
    inVals = symbols $ PlutusV2.valueSpent info

    -- If input has Issuer Token specified in ApprovalMinterParams,
    -- We allow temporary Approval Token to mint.
    -- To implement this, extend the mkMintingPolicy functions below, to include params.
    inputHasIssuerToken :: Bool
    inputHasIssuerToken = treasuryIssuerPID amp `elem` inVals

    checkTokenName :: Bool
    checkTokenName = valueOf (PlutusV2.txInfoMint info) (PlutusV2.ownCurrencySymbol ctx) tn > 0

{-
    As a Minting Policy
-}

policyV2 :: PSU.V2.MintingPolicy
policyV2 = PlutusV2.MintingPolicy $ PlutusV2.fromCompiledCode ($$(PlutusTx.compile [|| wrap ||]))
    where
        wrap amp = PSU.V2.mkUntypedMintingPolicy $ tokenNamePolicyV2 (PlutusTx.unsafeFromBuiltinData amp)

{-
    As a Script
-}

scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unMintingPolicyScript policyV2

{-
    As a Short Byte String
-}

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

{-
    As a Serialised Script
-}

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "output/test-policy.plutus" Nothing serialisedScriptV2
