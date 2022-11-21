{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ContributorToken.Compiler where

import              Prelude (FilePath, IO)
import              Cardano.Api
import              Cardano.Api.Shelley             ( PlutusScript (..), PlutusScriptV2 )
import              Codec.Serialise                 (serialise)
import              Data.Aeson
import qualified    Data.ByteString.Lazy            as LBS
import qualified    Data.ByteString.Short           as SBS
import qualified    Plutus.V1.Ledger.Scripts
import qualified    Plutus.V1.Ledger.Value
import qualified    Plutus.V2.Ledger.Api
import qualified    Plutus.V2.Ledger.Contexts
import qualified    PlutusTx
import              PlutusTx.Prelude


import qualified ContributorToken.ReferenceValidator as R
import ContributorToken.Types

-- If we do not import Ledger, then
-- how to replace Ledger.Validator?

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeTreasuryScript :: IO (Either (FileError ()) ())
writeTreasuryScript = writeValidator "output/reference-validator-v001.plutus" $ R.validator $ ReferenceParams
    {
      adminPolicyID       = "e0c7e0dc304da3c2e6a86bacda51e1e915c14732f12d4971dc380c15"
    , contributorPolicyID = "d8822958d5e0b8d1ad3b8c7e3840a03c3f0f201abdee7a6db4967153"
    }

